{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Common.Utils (defaultOllamaUrl, OllamaClient (..), encodeImage) where

import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Char (toLower)
import Data.Ollama.Common.Types
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
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
isSupportedExtension path = map toLower (takeExtension path) `elem` supportedExtensions

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
