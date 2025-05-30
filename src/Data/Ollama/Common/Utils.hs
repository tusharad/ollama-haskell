{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Ollama.Common.Utils
  ( defaultOllamaUrl
  , OllamaClient (..)
  , encodeImage
  , withOllamaRequest
  , commonNonStreamingHandler
  , commonStreamHandler
  , defaultModelOptions
  , withRetry
  , getVersion
  , nonJsonHandler
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as BSL
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Ollama.Common.Config
import Data.Ollama.Common.Error
import Data.Ollama.Common.Error qualified as Error
import Data.Ollama.Common.Types
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types (Status (statusCode))
import System.Directory
import System.FilePath

defaultOllamaUrl :: Text
defaultOllamaUrl = "http://127.0.0.1:11434"

supportedExtensions :: [String]
supportedExtensions = [".jpg", ".jpeg", ".png"]

safeReadFile :: FilePath -> IO (Either IOException BS.ByteString)
safeReadFile = try . BS.readFile

asPath :: FilePath -> IO (Maybe BS.ByteString)
asPath filePath = do
  exists <- doesFileExist filePath
  if exists
    then either (const Nothing) Just <$> safeReadFile filePath
    else return Nothing

isSupportedExtension :: FilePath -> Bool
isSupportedExtension p = map toLower (takeExtension p) `elem` supportedExtensions

{- |
  encodeImage is a utility function that takes an image file path (jpg, jpeg, png) and
  returns the image data in Base64 encoded format. Since GenerateOps' images field
  expects image data in base64. It is helper function that we are providing out of the box.
-}
encodeImage :: FilePath -> IO (Maybe Text)
encodeImage filePath = do
  if not (isSupportedExtension filePath)
    then return Nothing
    else do
      maybeContent <- asPath filePath
      return $ fmap (TE.decodeUtf8 . Base64.encode) maybeContent

withRetry :: Int -> Int -> IO (Either OllamaError a) -> IO (Either OllamaError a)
withRetry 0 _ action = action
withRetry retries delaySeconds action = do
  result <- action
  case result of
    Left err | isRetryableError err -> do
      threadDelay (delaySeconds * 1000000) -- Convert to microseconds
      withRetry (retries - 1) delaySeconds action
    _ -> return result
  where
    isRetryableError (HttpError _) = True
    isRetryableError (TimeoutError _) = True
    isRetryableError (JsonSchemaError _) = True
    isRetryableError (DecodeError _ _) = True
    isRetryableError _ = False

-- | Unified function for sending Ollama API requests
withOllamaRequest ::
  forall payload response.
  (ToJSON payload) =>
  -- | API endpoint (e.g., "/api/chat")
  Text ->
  -- | API method "POST" , "GET"
  BS.ByteString ->
  -- | Request body
  (Maybe payload) ->
  -- | Optional config (default: defaultConfig)
  Maybe OllamaConfig ->
  -- | Response handler
  (Response BodyReader -> IO (Either OllamaError response)) ->
  IO (Either OllamaError response)
withOllamaRequest endpoint reqMethod mbPayload mbOllamaConfig handler = do
  let OllamaConfig {..} = fromMaybe defaultOllamaConfig mbOllamaConfig
      fullUrl = T.unpack $ hostUrl <> endpoint
      timeoutMicros = timeout * 60 * 1000000

  manager <- case commonManager of
    Nothing ->
      newTlsManagerWith
        tlsManagerSettings
          { managerResponseTimeout = responseTimeoutMicro timeoutMicros
          }
    Just m -> pure m
  eRequest <- try $ parseRequest fullUrl :: IO (Either HttpException Request)
  case eRequest of
    Left ex -> return $ Left $ Error.HttpError ex
    Right req -> do
      let request =
            req
              { method = reqMethod
              , requestBody =
                  maybe
                    mempty
                    (\x -> RequestBodyLBS $ encode x)
                    mbPayload
              }
          retryCnt = fromMaybe 0 retryCount
          retryDelay_ = fromMaybe 1 retryDelay
      withRetry retryCnt retryDelay_ $ do
        fromMaybe (pure ()) onModelStart
        eResponse <- try $ withResponse request manager handler
        case eResponse of
          Left ex -> do
            fromMaybe (pure ()) onModelError
            return $ Left $ Error.HttpError ex
          Right result -> do
            fromMaybe (pure ()) onModelFinish
            return result

commonNonStreamingHandler ::
  FromJSON a =>
  Response BodyReader ->
  IO (Either OllamaError a)
commonNonStreamingHandler resp = do
  let bodyReader = responseBody resp
      respStatus = statusCode $ responseStatus resp
  if respStatus >= 200 && respStatus < 300
    then do
      -- Accumulate all chunks until EOF
      finalBs <- readFullBuff BS.empty bodyReader
      case eitherDecode (BSL.fromStrict finalBs) of
        Left err -> pure . Left $ Error.DecodeError err (show finalBs)
        Right decoded -> pure . Right $ decoded
    else (brRead bodyReader) >>= (pure . Left . ApiError . TE.decodeUtf8)

readFullBuff :: BS.ByteString -> BodyReader -> IO BS.ByteString
readFullBuff acc reader = do
      chunk <- brRead reader
      if BS.null chunk
        then pure acc
        else readFullBuff (acc `BS.append` chunk) reader

commonStreamHandler ::
  (HasDone a, FromJSON a) =>
  (a -> IO ()) ->
  IO () ->
  Response BodyReader ->
  IO (Either OllamaError a)
commonStreamHandler sendChunk flush resp = go mempty
  where
    go acc = do
      bs <- brRead $ responseBody resp
      if BS.null bs
        then do
          case eitherDecode (BSL.fromStrict acc) of
            Left err -> pure $ Left $ Error.DecodeError err (show acc)
            Right decoded -> pure $ Right decoded
        else do
          let chunk = BSL.fromStrict bs
          case eitherDecode chunk of
            Left err -> return $ Left $ Error.DecodeError err (show acc)
            Right res -> do
              sendChunk res
              flush
              if getDone res then return (Right res) else go (acc <> bs)

nonJsonHandler :: Response BodyReader -> IO (Either OllamaError BS.ByteString)
nonJsonHandler resp = do 
  let bodyReader = responseBody resp
      respStatus = statusCode $ responseStatus resp
  if respStatus >= 200 && respStatus < 300
    then readFullBuff BS.empty bodyReader >>= pure . Right
  else (brRead bodyReader) >>= (pure . Left . ApiError . TE.decodeUtf8)

defaultModelOptions :: ModelOptions
defaultModelOptions =
  ModelOptions
    { numKeep = Nothing
    , seed = Nothing
    , numPredict = Nothing
    , topK = Nothing
    , topP = Nothing
    , minP = Nothing
    , typicalP = Nothing
    , repeatLastN = Nothing
    , temperature = Nothing
    , repeatPenalty = Nothing
    , presencePenalty = Nothing
    , frequencyPenalty = Nothing
    , penalizeNewline = Nothing
    , stop = Nothing
    , numa = Nothing
    , numCtx = Nothing
    , numBatch = Nothing
    , numGpu = Nothing
    , mainGpu = Nothing
    , useMmap = Nothing
    , numThread = Nothing
    }

getVersion :: IO (Either OllamaError Version)
getVersion = do
  withOllamaRequest
    "/api/version"
    "GET"
    (Nothing :: Maybe Value)
    Nothing
    commonNonStreamingHandler
