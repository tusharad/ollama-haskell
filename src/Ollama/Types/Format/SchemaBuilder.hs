{- |
Module      : Ollama.Types.Format.SchemaBuilder
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

DSL for constructing structured JSON Schemas for Ollama's structured output API.

@since 1.0.0.0
-}
module Ollama.Types.Format.SchemaBuilder (
  JsonType (..),
  Property (..),
  Schema (..),
  SchemaBuilder,
  emptyObject,
  addProperty,
  addObjectProperty,
  requireField,
  requireFields,
  buildSchema,
  objectOf,
  arrayOf,
  printSchema,
  (|+),
  (|++),
  (|!),
  (|!!),
) where

import Data.Aeson
import Data.Map.Strict qualified as HM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TEncoding
import GHC.Generics (Generic)

{- | Supported JSON primitive and compound types.

@since 1.0.0.0
-}
data JsonType
  = JString
  | JNumber
  | JInteger
  | JBoolean
  | JNull
  | JArray !JsonType
  | JObject !Schema
  deriving stock (Show, Eq, Generic)

instance ToJSON JsonType where
  toJSON JString = "string"
  toJSON JNumber = "number"
  toJSON JInteger = "integer"
  toJSON JBoolean = "boolean"
  toJSON JNull = "null"
  toJSON (JArray _) = "array"
  toJSON (JObject _) = "object"

{- | Property metadata for schema properties.

@since 1.0.0.0
-}
newtype Property = Property JsonType
  deriving stock (Show, Eq, Generic)

instance ToJSON Property where
  toJSON (Property (JArray itemType)) =
    object ["type" .= ("array" :: Text), "items" .= Property itemType]
  toJSON (Property (JObject schema)) = toJSON schema
  toJSON (Property typ) = object ["type" .= typ]

{- | JSON schema specification.

@since 1.0.0.0
-}
data Schema = Schema
  { schemaProperties :: !(HM.Map Text Property)
  , schemaRequired :: ![Text]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Schema where
  toJSON (Schema props req) =
    object
      [ "type" .= ("object" :: Text)
      , "properties" .= props
      , "required" .= req
      ]

instance FromJSON JsonType where
  parseJSON = withText "JsonType" $ \case
    "string" -> pure JString
    "number" -> pure JNumber
    "integer" -> pure JInteger
    "boolean" -> pure JBoolean
    "null" -> pure JNull
    "array" -> pure (JArray JString)
    "object" -> pure (JObject (Schema HM.empty []))
    other -> fail $ "Unknown JsonType: " <> T.unpack other

instance FromJSON Property where
  parseJSON = withObject "Property" $ \v -> do
    t <- v .: "type"
    case (t :: Text) of
      "array" -> Property . JArray <$> (v .: "items" >>= parseJSON)
      "object" -> Property . JObject <$> parseJSON (Object v)
      _ -> Property <$> parseJSON (String t)

instance FromJSON Schema where
  parseJSON = withObject "Schema" $ \v ->
    Schema
      <$> v .:? "properties" .!= HM.empty
      <*> v .:? "required" .!= []

{- | Opaque builder for fluid schema construction.

@since 1.0.0.0
-}
newtype SchemaBuilder = SchemaBuilder Schema
  deriving stock (Show, Eq)

{- | Create an empty schema builder object.

@since 1.0.0.0
-}
emptyObject :: SchemaBuilder
emptyObject = SchemaBuilder $ Schema HM.empty []

{- | Add a primitive property to the schema builder.

@since 1.0.0.0
-}
addProperty :: Text -> JsonType -> SchemaBuilder -> SchemaBuilder
addProperty name typ (SchemaBuilder s) =
  SchemaBuilder $ s {schemaProperties = HM.insert name (Property typ) (schemaProperties s)}

{- | Add a nested object property to the schema builder.

@since 1.0.0.0
-}
addObjectProperty :: Text -> Schema -> SchemaBuilder -> SchemaBuilder
addObjectProperty name nestedSchema (SchemaBuilder s) =
  SchemaBuilder $
    s {schemaProperties = HM.insert name (Property (JObject nestedSchema)) (schemaProperties s)}

{- | Mark a field as required.

@since 1.0.0.0
-}
requireField :: Text -> SchemaBuilder -> SchemaBuilder
requireField name (SchemaBuilder s) =
  SchemaBuilder $ s {schemaRequired = name : schemaRequired s}

{- | Mark multiple fields as required.

@since 1.0.0.0
-}
requireFields :: [Text] -> SchemaBuilder -> SchemaBuilder
requireFields names builder = foldr requireField builder names

{- | Finalize a 'Schema' from a 'SchemaBuilder'.

@since 1.0.0.0
-}
buildSchema :: SchemaBuilder -> Schema
buildSchema (SchemaBuilder s) = s

{- | Treat a 'SchemaBuilder' as a nested object type.

@since 1.0.0.0
-}
objectOf :: SchemaBuilder -> JsonType
objectOf builder = JObject (buildSchema builder)

{- | Create an array schema type of an element type.

@since 1.0.0.0
-}
arrayOf :: JsonType -> JsonType
arrayOf = JArray

{- | Pretty-print a schema as formatted JSON.

@since 1.0.0.0
-}
printSchema :: Schema -> IO ()
printSchema = putStrLn . T.unpack . TL.toStrict . TEncoding.decodeUtf8 . encode

{- | Infix alias for 'addProperty'.

@since 1.0.0.0
-}
(|+) :: SchemaBuilder -> (Text, JsonType) -> SchemaBuilder
builder |+ (name, typ) = addProperty name typ builder

{- | Infix alias for 'addObjectProperty'.

@since 1.0.0.0
-}
(|++) :: SchemaBuilder -> (Text, Schema) -> SchemaBuilder
builder |++ (name, schema) = addObjectProperty name schema builder

{- | Infix alias for 'requireField'.

@since 1.0.0.0
-}
(|!) :: SchemaBuilder -> Text -> SchemaBuilder
builder |! name = requireField name builder

{- | Infix alias for 'requireFields'.

@since 1.0.0.0
-}
(|!!) :: SchemaBuilder -> [Text] -> SchemaBuilder
builder |!! names = requireFields names builder

infixl 7 |+, |++
infixl 6 |!, |!!
