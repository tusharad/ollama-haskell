{- |
Module      : Ollama.Client.Internal
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Low-level HTTP transport plumbing and request dispatchers.

@since 1.0.0.0
-}
module Ollama.Client.Internal (
  request,
  requestRaw,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client
import Network.HTTP.Types (statusCode)
import Ollama.Client (OllamaClient (..))
import Ollama.Client.Config (OllamaClientConfig (..))
import Ollama.Error (OllamaError (..))

{- | Dispatch non-streaming JSON request.

@since 1.0.0.0
-}
request ::
  (MonadIO m, ToJSON req, FromJSON resp) =>
  OllamaClient ->
  ByteString ->
  Text ->
  Maybe req ->
  m (Either OllamaError resp)
request OllamaClient {..} reqMethod endpoint mbPayload = liftIO $ do
  let fullUrl = T.unpack $ configBaseUrl clientConfig <> endpoint
  req <- parseRequest fullUrl
  let req' =
        req
          { method = reqMethod
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Accept", "application/json")
              ]
                ++ configHeaders clientConfig
          , requestBody = maybe mempty (RequestBodyLBS . encode) mbPayload
          }
  resp <- httpLbs req' clientManager
  let status = statusCode (responseStatus resp)
  if status >= 200 && status < 300
    then case eitherDecode (responseBody resp) of
      Left err -> pure $ Left $ DecodeError (T.pack err) (BSL.toStrict $ responseBody resp)
      Right val -> pure $ Right val
    else pure $ Left $ ApiError status (TE.decodeUtf8 . BSL.toStrict $ responseBody resp)

{- | Dispatch raw request returning raw bytes.

@since 1.0.0.0
-}
requestRaw ::
  (MonadIO m) =>
  OllamaClient ->
  ByteString ->
  Text ->
  Maybe ByteString ->
  m (Either OllamaError ByteString)
requestRaw OllamaClient {..} reqMethod endpoint mbPayload = liftIO $ do
  let fullUrl = T.unpack $ configBaseUrl clientConfig <> endpoint
  req <- parseRequest fullUrl
  let req' =
        req
          { method = reqMethod
          , requestHeaders = configHeaders clientConfig
          , requestBody = maybe mempty RequestBodyBS mbPayload
          }
  resp <- httpLbs req' clientManager
  let status = statusCode (responseStatus resp)
  if status >= 200 && status < 300
    then pure $ Right (BSL.toStrict $ responseBody resp)
    else pure $ Left $ ApiError status (TE.decodeUtf8 . BSL.toStrict $ responseBody resp)
