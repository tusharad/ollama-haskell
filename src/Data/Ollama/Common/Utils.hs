{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.Ollama.Common.Utils
  ( defaultOllamaUrl
  , OllamaClient (..)
  , encodeImage
  , withOllamaRequest
  , commonNonStreamingHandler
  , commonStreamHandler
  ) where

import Control.Exception (IOException, SomeException, try)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Base64 qualified as Base64
import Data.Char (toLower)
import Data.Maybe
import Data.Ollama.Common.Types
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client.TLS
import Network.HTTP.Client
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

-- | Unified function for sending Ollama API requests
withOllamaRequest ::
  forall payload response.
  (ToJSON payload, FromJSON response) =>
  -- | API endpoint (e.g., "/api/chat")
  Text ->
  -- | API method "POST" , "GET"
  BS.ByteString ->
  -- | Request body
  (Maybe payload) ->
  -- | Optional override for Ollama host URL
  Maybe Text ->
  -- | Optional timeout in minutes (default: 15)
  Maybe Int ->
  -- | Response handler
  (FromJSON response => Response BodyReader -> IO (Either String response)) ->
  IO (Either String response)
withOllamaRequest endpoint reqMethod mbPayload hostUrl mTimeout handler = do
  let url = fromMaybe defaultOllamaUrl hostUrl
      fullUrl = T.unpack $ url <> endpoint
      timeoutMicros = fromMaybe (15 * 60 * 1000000) (fmap (\x -> x * 60 * 1000000) mTimeout)

  manager <-
    newTlsManagerWith
      tlsManagerSettings
        { managerResponseTimeout = responseTimeoutMicro timeoutMicros
        }
  eRequest <- try $ parseRequest fullUrl :: IO (Either HttpException Request)
  case eRequest of
    Left ex -> return $ Left $ "Failed to parse request URL: " <> show ex
    Right req -> do
      let request =
            req
              { method = reqMethod
              , requestBody = maybe mempty (\x -> RequestBodyLBS $ encode x) mbPayload
              }
      eResponse <- try $ withResponse request manager handler
      case eResponse of
        Left ex -> return $ Left $ "HTTP error occurred: " <> show (ex :: SomeException)
        Right result -> return result

commonNonStreamingHandler ::
  FromJSON a =>
  Response BodyReader ->
  IO (Either String a)
commonNonStreamingHandler resp = do
  let bodyReader = responseBody resp
  -- Accumulate all chunks until EOF
  finalBs <- go BS.empty bodyReader
  case eitherDecode (BSL.fromStrict finalBs) of
    Left err -> pure $ Left ("Failed to decode JSON response: " <> err <> show finalBs)
    Right decoded -> pure $ Right decoded
  where
    go :: BS.ByteString -> BodyReader -> IO BS.ByteString
    go acc reader = do
      chunk <- brRead reader
      if BS.null chunk
        then pure acc
        else go (acc `BS.append` chunk) reader

commonStreamHandler ::
  (HasDone a, FromJSON a) =>
  (a -> IO ()) ->
  IO () ->
  Response BodyReader ->
  IO (Either String a)
commonStreamHandler sendChunk flush resp = go mempty
  where
    go acc = do
      bs <- brRead $ responseBody resp
      if BS.null bs
        then do
          case eitherDecode (BSL.fromStrict acc) of
            Left err -> pure $ Left $ "Empty or invalid response " <> err <> show acc
            Right decoded -> pure $ Right decoded
        else do
          let chunk = BSL.fromStrict bs
          case decode chunk of
            Nothing -> return $ Left ("Failed to decode chunk: " <> show bs)
            Just res -> do
              sendChunk res
              flush
              if getDone res then return (Right res) else go (acc <> bs)
