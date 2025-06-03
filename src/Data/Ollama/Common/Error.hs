{-# LANGUAGE DeriveGeneric #-}
{- |
Module:      Data.Ollama.Common.Error
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Description: Defination of Unified error type for Ollama
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental
-}
module Data.Ollama.Common.Error (
    OllamaError (..)
  , DecodingErrorMessage 
  , DecodingFailedValue
  ) where

import Control.Exception (Exception, IOException)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client

type DecodingErrorMessage = String
type DecodingFailedValue = String

data OllamaError
  = HttpError HttpException
  | DecodeError DecodingErrorMessage DecodingFailedValue 
  | ApiError Text
  | FileError IOException
  | JsonSchemaError String
  | TimeoutError String
  | InvalidRequest String
  deriving (Show, Generic)

instance Eq OllamaError where
  (HttpError _) == (HttpError _) = True
  x == y = eqOllamaError x y
    where
      eqOllamaError :: OllamaError -> OllamaError -> Bool
      eqOllamaError = (==)

instance Exception OllamaError
