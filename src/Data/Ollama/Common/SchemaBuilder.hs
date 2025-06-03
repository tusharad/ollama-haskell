{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Data.Ollama.Common.SchemaBuilder
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Description: Interface you providing schema for structured output
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental
-}
module Data.Ollama.Common.SchemaBuilder
  ( JsonType (..)
  , Property (..)
  , Schema (..)
  , emptyObject
  , addProperty
  , addObjectProperty
  , requireField
  , requireFields
  , buildSchema
  , objectOf
  , arrayOf
  , toOllamaFormat
  , printSchema
  , (|+)
  , (|++)
  , (|!)
  , (|!!)
  ) where

import Data.Aeson
import Data.Map.Strict qualified as HM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as T
import GHC.Generics

-- | JSON Schema types
data JsonType
  = JString
  | JNumber
  | JInteger
  | JBoolean
  | JNull
  | JArray JsonType -- Array of specific type
  | JObject Schema -- Nested object with its own schema
  deriving (Show, Eq, Generic)

instance ToJSON JsonType where
  toJSON JString = "string"
  toJSON JNumber = "number"
  toJSON JInteger = "integer"
  toJSON JBoolean = "boolean"
  toJSON JNull = "null"
  toJSON (JArray _) = "array"
  toJSON (JObject _) = "object"

-- | Simple property with only type (but type can now be nested)
newtype Property = Property JsonType
  deriving (Show, Eq, Generic)

instance ToJSON Property where
  toJSON (Property (JArray itemType)) =
    object
      [ "type" .= ("array" :: Text)
      , "items" .= Property itemType
      ]
  toJSON (Property (JObject schema)) = toJSON schema
  toJSON (Property typ) = object ["type" .= typ]

-- | Complete JSON Schema - now recursive
data Schema = Schema
  { schemaProperties :: HM.Map Text Property
  , schemaRequired :: [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Schema where
  toJSON (Schema props req) =
    object
      [ "type" .= ("object" :: Text)
      , "properties" .= props
      , "required" .= req
      ]

-- | Schema builder
newtype SchemaBuilder = SchemaBuilder Schema
  deriving (Show, Eq)

-- | Create empty object schema
emptyObject :: SchemaBuilder
emptyObject = SchemaBuilder $ Schema HM.empty []

-- | Add a property
addProperty :: Text -> JsonType -> SchemaBuilder -> SchemaBuilder
addProperty name typ (SchemaBuilder s) =
  SchemaBuilder $
    s
      { schemaProperties = HM.insert name (Property typ) (schemaProperties s)
      }

-- | Add a nested object property
addObjectProperty :: Text -> Schema -> SchemaBuilder -> SchemaBuilder
addObjectProperty name nestedSchema (SchemaBuilder s) =
  SchemaBuilder $
    s
      { schemaProperties =
          HM.insert name (Property (JObject nestedSchema)) (schemaProperties s)
      }

-- | Mark field as required
requireField :: Text -> SchemaBuilder -> SchemaBuilder
requireField name (SchemaBuilder s) =
  SchemaBuilder $
    s
      { schemaRequired = name : schemaRequired s
      }

-- | Mark multiple fields as required
requireFields :: [Text] -> SchemaBuilder -> SchemaBuilder
requireFields names builder = foldr requireField builder names

-- | Build final schema
buildSchema :: SchemaBuilder -> Schema
buildSchema (SchemaBuilder s) = s

-- | Helper functions for common nested patterns
objectOf :: SchemaBuilder -> JsonType
objectOf builder = JObject (buildSchema builder)

arrayOf :: JsonType -> JsonType
arrayOf = JArray

-- | Convert Schema to JSON for API calls
toOllamaFormat :: Schema -> Value
toOllamaFormat = toJSON

-- | Pretty print schema as JSON
printSchema :: Schema -> IO ()
printSchema = putStrLn . T.unpack . TL.toStrict . T.decodeUtf8 . encode

-- | Infix operators for fluent interface
(|+) :: SchemaBuilder -> (Text, JsonType) -> SchemaBuilder
builder |+ (name, typ) = addProperty name typ builder

-- | Add nested object property
(|++) :: SchemaBuilder -> (Text, Schema) -> SchemaBuilder
builder |++ (name, schema) = addObjectProperty name schema builder

-- | Mark field as required
(|!) :: SchemaBuilder -> Text -> SchemaBuilder
builder |! name = requireField name builder

-- | Mark multiple fields as required
(|!!) :: SchemaBuilder -> [Text] -> SchemaBuilder
builder |!! names = requireFields names builder

infixl 7 |+, |++
infixl 6 |!, |!!
