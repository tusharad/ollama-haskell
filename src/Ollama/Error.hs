{- |
Module      : Ollama.Error
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Core error type definitions for the Ollama library.

@since 3.0.0.0
-}
module Ollama.Error (
  OllamaError (..),
  isRetryable,
  throwOllama,
) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Client (HttpException)

{- | Unified error type representing all failure modes in the Ollama client.

@since 3.0.0.0
-}
data OllamaError
  = -- | HTTP transport failure (connection error, DNS failure, etc.)
    HttpError !HttpException
  | -- | Ollama API returned an error response (HTTP status code + message)
    ApiError !Int !Text
  | -- | Failed to decode JSON response body
    DecodeError !Text !ByteString
  | -- | Request timed out waiting for a response
    TimeoutError
  | -- | Client-side validation failure before sending the request
    InvalidRequest !Text
  deriving stock (Show)

instance Exception OllamaError

instance Eq OllamaError where
  ApiError s1 t1 == ApiError s2 t2 = s1 == s2 && t1 == t2
  DecodeError t1 _ == DecodeError t2 _ = t1 == t2
  TimeoutError == TimeoutError = True
  InvalidRequest t1 == InvalidRequest t2 = t1 == t2
  HttpError _ == HttpError _ = False
  _ == _ = False

{- | Determine whether an error is transient and safe to retry.

@since 3.0.0.0
-}
isRetryable :: OllamaError -> Bool
isRetryable (HttpError _) = True
isRetryable TimeoutError = True
isRetryable (ApiError status _) | status >= 500 = True
isRetryable _ = False

{- | Helper to throw an 'OllamaError' as an exception.

@since 3.0.0.0
-}
throwOllama :: OllamaError -> IO a
throwOllama = throwIO
