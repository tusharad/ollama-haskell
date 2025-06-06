{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Data.Ollama.Common.Utils
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Utility functions for interacting with the Ollama API, including image encoding, HTTP request handling, and retry logic.

This module provides helper functions for common tasks in the Ollama client, such as encoding images to Base64,
sending HTTP requests to the Ollama API, handling streaming and non-streaming responses, and managing retries for failed requests.
It also includes a default model options configuration and a function to retrieve the Ollama server version.

The functions in this module are used internally by other modules like 'Data.Ollama.Chat' and 'Data.Ollama.Generate' but can also be used directly for custom API interactions.
-}
module Data.Ollama.Common.Utils
  ( -- * Image Encoding
    encodeImage

    -- * HTTP Request Handling
  , withOllamaRequest
  , commonNonStreamingHandler
  , commonStreamHandler
  , nonJsonHandler

    -- * Model Options
  , defaultModelOptions

    -- * Retry Logic
  , withRetry

    -- * Version Retrieval
  , getVersion
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

-- | List of supported image file extensions for 'encodeImage'.
supportedExtensions :: [String]
supportedExtensions = [".jpg", ".jpeg", ".png"]

-- | Safely read a file, returning an 'Either' with an 'IOException' on failure.
safeReadFile :: FilePath -> IO (Either IOException BS.ByteString)
safeReadFile = try . BS.readFile

-- | Read a file if it exists, returning 'Nothing' if it does not.
asPath :: FilePath -> IO (Maybe BS.ByteString)
asPath filePath = do
  exists <- doesFileExist filePath
  if exists
    then either (const Nothing) Just <$> safeReadFile filePath
    else return Nothing

-- | Check if a file has a supported image extension.
isSupportedExtension :: FilePath -> Bool
isSupportedExtension p = map toLower (takeExtension p) `elem` supportedExtensions

{- | Encodes an image file to Base64 format.

Takes a file path to an image (jpg, jpeg, or png) and returns its data encoded as a Base64 'Text'.
Returns 'Nothing' if the file extension is unsupported or the file cannot be read.
This is useful for including images in API requests that expect Base64-encoded data, such as 'GenerateOps' images field.
-}
encodeImage :: FilePath -> IO (Maybe Text)
encodeImage filePath = do
  if not (isSupportedExtension filePath)
    then return Nothing
    else do
      maybeContent <- asPath filePath
      return $ fmap (TE.decodeUtf8 . Base64.encode) maybeContent

{- | Executes an action with retry logic for recoverable errors.

Retries the given action up to the specified number of times with a delay (in seconds) between attempts.
Only retries on recoverable errors such as HTTP errors, timeouts, JSON schema errors, or decoding errors.
-}
withRetry ::
  -- | Number of retries
  Int ->
  -- | Delay between retries in seconds
  Int ->
  -- | Action to execute, returning 'Either' 'OllamaError' or a result
  IO (Either OllamaError a) ->
  IO (Either OllamaError a)
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

{- | Sends an HTTP request to the Ollama API.

A unified function for making API requests to the Ollama server. Supports both GET and POST methods,
customizable payloads, and optional configuration. The response is processed by the provided handler.
-}
withOllamaRequest ::
  forall payload response.
  (ToJSON payload) =>
  -- | API endpoint
  Text ->
  -- | HTTP method ("GET" or "POST")
  BS.ByteString ->
  -- | Optional request payload (must implement 'ToJSON')
  Maybe payload ->
  -- | Optional 'OllamaConfig' (defaults to 'defaultOllamaConfig')
  Maybe OllamaConfig ->
  -- | Response handler to process the HTTP response
  (Response BodyReader -> IO (Either OllamaError response)) ->
  IO (Either OllamaError response)
withOllamaRequest endpoint reqMethod mbPayload mbOllamaConfig handler = do
  let OllamaConfig {..} = fromMaybe defaultOllamaConfig mbOllamaConfig
      fullUrl = T.unpack $ hostUrl <> endpoint
      timeoutMicros = timeout * 1000000
  manager <- case commonManager of
    Nothing ->
      newTlsManagerWith
        tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro timeoutMicros}
    Just m -> pure m
  eRequest <- try $ parseRequest fullUrl
  case eRequest of
    Left ex -> return $ Left $ Error.HttpError ex
    Right req -> do
      let request =
            req
              { method = reqMethod
              , requestBody =
                  maybe mempty (\x -> RequestBodyLBS $ encode x) mbPayload
              }
          retryCnt = fromMaybe 0 retryCount
          retryDelay_ = fromMaybe 1 retryDelay
      withRetry retryCnt retryDelay_ $ do
        fromMaybe (pure ()) onModelStart
        eResponse <- try $ withResponse request manager handler
        case eResponse of
          Left ex -> do
            fromMaybe (pure ()) onModelError
            case ex of
              (HttpExceptionRequest _ ResponseTimeout) ->
                return $ Left $ Error.TimeoutError "No response from LLM yet"
              _ -> return $ Left $ Error.HttpError ex
          Right result -> do
            fromMaybe (pure ()) onModelFinish
            return result

{- | Handles non-streaming API responses.

Processes an HTTP response, accumulating all chunks until EOF and decoding the result as JSON.
Returns an 'Either' with an 'OllamaError' on failure or the decoded response on success.
Suitable for APIs that return a single JSON response.
-}
commonNonStreamingHandler ::
  FromJSON a =>
  Response BodyReader ->
  IO (Either OllamaError a)
commonNonStreamingHandler resp = do
  let bodyReader = responseBody resp
      respStatus = statusCode $ responseStatus resp
  if respStatus >= 200 && respStatus < 300
    then do
      finalBs <- readFullBuff BS.empty bodyReader
      case eitherDecode (BSL.fromStrict finalBs) of
        Left err -> pure . Left $ Error.DecodeError err (show finalBs)
        Right decoded -> pure . Right $ decoded
    else Left . ApiError . TE.decodeUtf8 <$> brRead bodyReader

{- | Accumulates response chunks into a single ByteString.

Internal helper function to read all chunks from a 'BodyReader' until EOF.
-}
readFullBuff :: BS.ByteString -> BodyReader -> IO BS.ByteString
readFullBuff acc reader = do
  chunk <- brRead reader
  if BS.null chunk
    then pure acc
    else readFullBuff (acc `BS.append` chunk) reader

{- | Handles streaming API responses.

Processes a streaming HTTP response, decoding each chunk as JSON and passing it to the provided
'sendChunk' function. The 'flush' function is called after each chunk. Stops when the response
indicates completion (via 'HasDone'). Returns the final decoded response or an error.
-}
commonStreamHandler ::
  (HasDone a, FromJSON a) =>
  -- | Function to handle each decoded chunk
  (a -> IO ()) ->
  -- | Function to flush after each chunk
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

{- | Handles non-JSON API responses.

Processes an HTTP response, accumulating all chunks into a 'ByteString'. Returns the accumulated
data on success (HTTP status 2xx) or an 'ApiError' on failure.
-}
nonJsonHandler :: Response BodyReader -> IO (Either OllamaError BS.ByteString)
nonJsonHandler resp = do
  let bodyReader = responseBody resp
      respStatus = statusCode $ responseStatus resp
  if respStatus >= 200 && respStatus < 300
    then Right <$> readFullBuff BS.empty bodyReader
    else Left . ApiError . TE.decodeUtf8 <$> brRead bodyReader

{- | Default model options for API requests.

Provides a default 'ModelOptions' configuration with all fields set to 'Nothing',
suitable as a starting point for customizing model parameters like temperature or token limits.

Example:

>>> let opts = defaultModelOptions { temperature = Just 0.7 }
-}
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

{- | Retrieves the Ollama server version.

Sends a GET request to the @\/api\/version@ endpoint and returns the server version
as a 'Version' wrapped in an 'Either' 'OllamaError'.

Example:

>>> getVersion
--
-- @since 0.2.0.0
-}
getVersion :: IO (Either OllamaError Version)
getVersion = do
  withOllamaRequest
    "/api/version"
    "GET"
    (Nothing :: Maybe Value)
    Nothing
    commonNonStreamingHandler
