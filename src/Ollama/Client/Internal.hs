{- |
Module      : Ollama.Client.Internal
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Low-level HTTP transport plumbing and request dispatchers.

Implements retry logic, lifecycle callbacks, structured logging,
authentication header injection, and conduit-based response streaming.

@since 1.0.0.0
-}
module Ollama.Client.Internal (
  request,
  requestRaw,
  requestStreaming,
) where

import Conduit (
  ConduitT,
  awaitForever,
  bracketP,
  filterC,
  takeWhileC,
  transPipe,
  yield,
  (.|),
 )
import Control.Exception (SomeException, catch, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Retry qualified as Retry
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive (CI)
import Data.Conduit.Binary qualified as CB
import Data.Conduit.Combinators (repeatM)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client
import Network.HTTP.Types (statusCode)
import Ollama.Client (OllamaClient (..))
import Ollama.Client.Config (LogLevel (..), OllamaClientConfig (..), RetryPolicy (..))
import Ollama.Error (OllamaError (..), isRetryable)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

{- | Dispatch a non-streaming JSON API request.

Applies authentication headers, retry policy, lifecycle callbacks,
and structured logging as configured on the client.

@since 1.0.0.0
-}
request ::
  (MonadIO m, ToJSON req, FromJSON resp) =>
  OllamaClient ->
  ByteString ->
  Text ->
  Maybe req ->
  m (Either OllamaError resp)
request client reqMethod endpoint mbPayload = liftIO $
  withRetry client endpoint $
    executeJsonRequest client reqMethod endpoint mbPayload

{- | Dispatch a raw (non-JSON) request returning raw bytes.

Used for blob endpoints where the response body is not JSON.
Applies authentication headers, retry policy, lifecycle callbacks,
and structured logging.

@since 1.0.0.0
-}
requestRaw ::
  (MonadIO m) =>
  OllamaClient ->
  ByteString ->
  Text ->
  Maybe ByteString ->
  m (Either OllamaError ByteString)
requestRaw client reqMethod endpoint mbPayload = liftIO $
  withRetry client endpoint $
    executeRawRequest client reqMethod endpoint mbPayload

{- | Dispatch a conduit-based streaming API request.

Opens an HTTP response stream, reads line-delimited JSON chunks as they arrive,
decodes each chunk, and yields values into a 'ConduitT'. Stops when the server
closes the connection.

@since 1.0.0.0
-}
requestStreaming ::
  (MonadUnliftIO m, ToJSON req, FromJSON resp) =>
  OllamaClient ->
  Text ->
  req ->
  ConduitT () resp m ()
requestStreaming OllamaClient {..} endpoint payload = do
  let fullUrl = T.unpack $ configBaseUrl clientConfig <> endpoint
      cfg = clientConfig
  initReq <- liftIO $ parseRequest fullUrl
  let req =
        initReq
          { method = "POST"
          , responseTimeout = responseTimeoutNone
          , requestHeaders =
              [("Content-Type", "application/json")]
                ++ authHeader cfg
                ++ configHeaders cfg
          , requestBody = RequestBodyLBS (encode payload)
          }
  transPipe runResourceT $
    bracketP
      (responseOpen req clientManager)
      responseClose
      ( \resp -> do
          let bodyReader = responseBody resp
              source = repeatM (liftIO $ brRead bodyReader) .| takeWhileC (not . BS.null)
          source .| CB.lines .| filterC (not . BS.null) .| parseAndYield
      )
  where
    parseAndYield = awaitForever $ \line -> do
      case eitherDecode (BSL.fromStrict line) of
        Left _err -> pure ()
        Right val -> yield val

-- ---------------------------------------------------------------------------
-- Core request execution
-- ---------------------------------------------------------------------------

{- | Execute a JSON request without retry wrapping. -}
executeJsonRequest ::
  (ToJSON req, FromJSON resp) =>
  OllamaClient ->
  ByteString ->
  Text ->
  Maybe req ->
  IO (Either OllamaError resp)
executeJsonRequest OllamaClient {..} reqMethod endpoint mbPayload = do
  let fullUrl = T.unpack $ configBaseUrl clientConfig <> endpoint
      cfg = clientConfig
  initReq <- parseRequest fullUrl
  let timeoutMicro = configTimeout cfg * 1000000
      req =
        initReq
          { method = reqMethod
          , responseTimeout = responseTimeoutMicro timeoutMicro
          , requestHeaders =
              [("Content-Type", "application/json"), ("Accept", "application/json")]
                ++ authHeader cfg
                ++ configHeaders cfg
          , requestBody = maybe mempty (RequestBodyLBS . encode) mbPayload
          }
  result <- try @HttpException $ httpLbs req clientManager
  case result of
    Left httpErr -> pure $ Left $ HttpError httpErr
    Right resp -> do
      let status = statusCode (responseStatus resp)
          body = responseBody resp
      if status >= 200 && status < 300
        then
          if BSL.null body
            then case eitherDecode "null" of
              Left err -> pure $ Left $ DecodeError (T.pack err) (BSL.toStrict body)
              Right val -> pure $ Right val
            else case eitherDecode body of
              Left err -> pure $ Left $ DecodeError (T.pack err) (BSL.toStrict body)
              Right val -> pure $ Right val
        else pure $ Left $ ApiError status (TE.decodeUtf8 . BSL.toStrict $ body)

{- | Execute a raw byte request without retry wrapping. -}
executeRawRequest ::
  OllamaClient ->
  ByteString ->
  Text ->
  Maybe ByteString ->
  IO (Either OllamaError ByteString)
executeRawRequest OllamaClient {..} reqMethod endpoint mbPayload = do
  let fullUrl = T.unpack $ configBaseUrl clientConfig <> endpoint
      cfg = clientConfig
  initReq <- parseRequest fullUrl
  let timeoutMicro = configTimeout cfg * 1000000
      req =
        initReq
          { method = reqMethod
          , responseTimeout = responseTimeoutMicro timeoutMicro
          , requestHeaders = authHeader cfg ++ configHeaders cfg
          , requestBody = maybe mempty RequestBodyBS mbPayload
          }
  result <- try @HttpException $ httpLbs req clientManager
  case result of
    Left httpErr -> pure $ Left $ HttpError httpErr
    Right resp -> do
      let status = statusCode (responseStatus resp)
      if status >= 200 && status < 300
        then pure $ Right (BSL.toStrict $ responseBody resp)
        else pure $ Left $ ApiError status (TE.decodeUtf8 . BSL.toStrict $ body)
        where
          body = responseBody resp

-- ---------------------------------------------------------------------------
-- Retry logic
-- ---------------------------------------------------------------------------

{- | Wrap an IO action with the client's configured retry policy, lifecycle
callbacks, and structured logging.

Only retries when 'isRetryable' returns 'True' for the error.
-}
withRetry ::
  OllamaClient ->
  Text ->
  IO (Either OllamaError a) ->
  IO (Either OllamaError a)
withRetry OllamaClient {clientConfig = cfg} endpoint action = do
  let policy = toRetryPolicy (configRetry cfg)
      shouldRetry _status (Left err) = do
        logMsg cfg Warn $ "Retryable error on " <> endpoint <> ", will retry: " <> T.pack (show err)
        pure $ isRetryable err
      shouldRetry _status (Right _) = pure False
  fireCallback (configOnStart cfg)
  logMsg cfg Debug $ "Requesting " <> endpoint
  result <- Retry.retrying policy shouldRetry (const action)
  case result of
    Left err -> do
      fireCallback (configOnError cfg)
      logMsg cfg Error $ "Request failed: " <> endpoint <> " — " <> T.pack (show err)
    Right _ -> do
      fireCallback (configOnSuccess cfg)
      logMsg cfg Info $ "Request succeeded: " <> endpoint
  pure result

{- | Map our 'RetryPolicy' ADT to the @retry@ package's 'Retry.RetryPolicyM'. -}
toRetryPolicy :: RetryPolicy -> Retry.RetryPolicyM IO
toRetryPolicy NoRetry = Retry.limitRetries 0
toRetryPolicy (ConstantRetry count delaySec) =
  Retry.constantDelay (delaySec * 1000000) <> Retry.limitRetries count
toRetryPolicy (ExponentialRetry count initialDelayMs) =
  Retry.exponentialBackoff (initialDelayMs * 1000) <> Retry.limitRetries count

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

{- | Build the Authorization header if an API key is configured. -}
authHeader :: OllamaClientConfig -> [(CI ByteString, ByteString)]
authHeader cfg = case configApiKey cfg of
  Nothing -> []
  Just key -> [("Authorization", "Bearer " <> TE.encodeUtf8 key)]

{- | Fire an optional callback, silently ignoring exceptions. -}
fireCallback :: Maybe (IO ()) -> IO ()
fireCallback Nothing = pure ()
fireCallback (Just cb) = cb `catch` \(_ :: SomeException) -> pure ()

{- | Log a message via the configured logger, if present. -}
logMsg :: OllamaClientConfig -> LogLevel -> Text -> IO ()
logMsg cfg level msg = case configLogger cfg of
  Nothing -> pure ()
  Just logger -> logger level msg `catch` \(_ :: SomeException) -> pure ()
