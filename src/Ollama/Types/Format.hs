{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Module      : Ollama.Types.Format
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Response formatting specifications for structured model output.

@since 1.0.0.0
-}
module Ollama.Types.Format (
  Format (..),
  formatFor,
  module Ollama.Types.Format.SchemaBuilder,
  module Ollama.Types.Format.SchemaDerive,
) where

import Data.Aeson
import Ollama.Types.Format.SchemaBuilder
import Ollama.Types.Format.SchemaDerive

{- | Response output format hint.

@since 1.0.0.0
-}
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

{- | Produce a 'Format' value suitable for the @genFormat@ \/ @chatFormat@
request fields.

@
req = (generateRequest model prompt)
        { genFormat = Just ('formatFor' \@Person) }
@

@since 0.4.0.0
-}
formatFor :: forall a. (ToSchema a) => Format
formatFor = SchemaFormat (schemaFor @a)
