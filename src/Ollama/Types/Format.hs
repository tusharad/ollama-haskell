-- |
-- Module      : Ollama.Types.Format
-- Copyright   : (c) 2024-2026 Tushar Adhatrao
-- License     : MIT
-- Maintainer  : tusharadhatrao@gmail.com
-- Stability   : stable
-- Portability : portable
--
-- Response formatting specifications for structured model output.
--
-- @since 1.0.0.0
module Ollama.Types.Format
  ( Format (..)
  , module Ollama.Types.Format.SchemaBuilder
  ) where

import Data.Aeson
import Ollama.Types.Format.SchemaBuilder

-- | Response output format hint.
--
-- @since 1.0.0.0
data Format
  = -- | Constrain response output to generic valid JSON
    JsonFormat
  | -- | Constrain response output to a specific JSON Schema
    SchemaFormat !Schema
  deriving stock (Eq, Show)

instance ToJSON Format where
  toJSON JsonFormat = String "json"
  toJSON (SchemaFormat sch) = toJSON sch

instance FromJSON Format where
  parseJSON (String "json") = pure JsonFormat
  parseJSON v = SchemaFormat <$> parseJSON v
