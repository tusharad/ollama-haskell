{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Data.Ollama.Common.Error
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Unified error type for handling failures across the Ollama client.

== Overview

Defines the core 'OllamaError' type that wraps all potential errors
encountered while interacting with the Ollama API, including HTTP errors,
JSON decoding failures, API-specific errors, file I/O errors, and timeouts.
-}
module Data.Ollama.Common.Error
  ( -- * Error Types
    OllamaError (..)

    -- * Decoding Utilities
  , DecodingErrorMessage
  , DecodingFailedValue
  ) where

import Control.Exception (Exception, IOException)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (HttpException)

-- | Type alias for a decoding error message string.
type DecodingErrorMessage = String

-- | Type alias for the value that failed to decode.
type DecodingFailedValue = String

-- | Represents all possible errors that may occur when using the Ollama client.
--
-- @since 0.2.0.0
data OllamaError
  = -- | Low-level HTTP exception (connection failure, etc.)
    HttpError HttpException
  | -- | Failure to decode a JSON response, includes message and raw value
    DecodeError DecodingErrorMessage DecodingFailedValue
  | -- | Error returned from Ollama's HTTP API
    ApiError Text
  | -- | Error during file operations (e.g., loading an image)
    FileError IOException
  | -- | Mismatch in expected JSON schema or structure
    JsonSchemaError String
  | -- | Request timed out
    TimeoutError String
  | -- | Request is malformed or violates input constraints
    InvalidRequest String
  deriving (Show, Generic)

instance Eq OllamaError where
  (HttpError _) == (HttpError _) = True
  x == y = eqOllamaError x y
    where
      eqOllamaError :: OllamaError -> OllamaError -> Bool
      eqOllamaError = (==)

instance Exception OllamaError
