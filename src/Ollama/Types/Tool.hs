{-# LANGUAGE PatternSynonyms #-}
{- |
Module      : Ollama.Types.Tool
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Tool calling and structured function interfaces for the Ollama API.

@since 3.0.0.0
-}
module Ollama.Types.Tool (
  Tool (..),
  FunctionDef (..),
  FunctionParameters (..),
  ToolCall (.., ToolCall),
  ToolCallFunction (.., ToolCallFunction),
  mkToolCall,
  toolCallWithId,
  mkToolCallFunction,
  objectParams,
  stringParam,
  numberParam,
  intParam,
  boolParam,
  enumParam,
  functionDef,
  toolDef,
) where

import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

{- | Tool definition provided to the model.

@since 3.0.0.0
-}
data Tool = Tool
  { toolType :: !Text
  , toolFunction :: !FunctionDef
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Tool where
  toJSON Tool {..} =
    object
      [ "type" .= toolType
      , "function" .= toolFunction
      ]

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \v ->
    Tool
      <$> v .: "type"
      <*> v .: "function"

{- | Definition of a function that can be called by the model.

@since 3.0.0.0
-}
data FunctionDef = FunctionDef
  { fnName :: !Text
  , fnDescription :: !(Maybe Text)
  , fnParameters :: !(Maybe FunctionParameters)
  , fnStrict :: !(Maybe Bool)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FunctionDef where
  toJSON FunctionDef {..} =
    object $
      ["name" .= fnName]
        ++ maybe [] (\d -> ["description" .= d]) fnDescription
        ++ maybe [] (\p -> ["parameters" .= p]) fnParameters
        ++ maybe [] (\s -> ["strict" .= s]) fnStrict

instance FromJSON FunctionDef where
  parseJSON = withObject "FunctionDef" $ \v ->
    FunctionDef
      <$> v .: "name"
      <*> v .:? "description"
      <*> v .:? "parameters"
      <*> v .:? "strict"

{- | Parameters schema for a function call.

@since 3.0.0.0
-}
data FunctionParameters = FunctionParameters
  { fpType :: !Text
  , fpProperties :: !(Maybe (Map Text FunctionParameters))
  , fpRequired :: !(Maybe [Text])
  , fpAdditionalProperties :: !(Maybe Bool)
  , fpDescription :: !(Maybe Text)
  , fpEnum :: !(Maybe [Text])
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FunctionParameters where
  toJSON FunctionParameters {..} =
    object $
      ["type" .= fpType]
        ++ maybe [] (\p -> ["properties" .= p]) fpProperties
        ++ maybe [] (\r -> ["required" .= r]) fpRequired
        ++ maybe [] (\a -> ["additionalProperties" .= a]) fpAdditionalProperties
        ++ maybe [] (\d -> ["description" .= d]) fpDescription
        ++ maybe [] (\e -> ["enum" .= e]) fpEnum

instance FromJSON FunctionParameters where
  parseJSON = withObject "FunctionParameters" $ \v ->
    FunctionParameters
      <$> v .: "type"
      <*> v .:? "properties"
      <*> v .:? "required"
      <*> v .:? "additionalProperties"
      <*> v .:? "description"
      <*> v .:? "enum"

{- | Tool call returned in model's assistant response.

@since 3.0.0.0
-}
data ToolCall = ToolCall'
  { tcFunction :: !ToolCallFunction
  , tcId :: !(Maybe Text)
  , tcType :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

-- | Pattern synonym providing backwards compatibility with 1-argument construction
-- and pattern matching.
--
-- @since 0.4.2.0
pattern ToolCall :: ToolCallFunction -> ToolCall
pattern ToolCall fn <- ToolCall' fn _ _
  where
    ToolCall fn = ToolCall' fn Nothing Nothing
{-# COMPLETE ToolCall #-}

-- | Construct a 'ToolCall' with only the function payload.
--
-- @since 0.4.2.0
mkToolCall :: ToolCallFunction -> ToolCall
mkToolCall fn = ToolCall' fn Nothing Nothing

-- | Construct a 'ToolCall' with an identifier, type, and function payload.
--
-- @since 0.4.2.0
toolCallWithId :: Text -> ToolCallFunction -> ToolCall
toolCallWithId callId fn = ToolCall' fn (Just callId) (Just "function")

instance ToJSON ToolCall where
  toJSON ToolCall' {..} =
    object $
      ["function" .= tcFunction]
        ++ maybe [] (\i -> ["id" .= i]) tcId
        ++ maybe [] (\t -> ["type" .= t]) tcType

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \v ->
    ToolCall'
      <$> v .: "function"
      <*> v .:? "id"
      <*> v .:? "type"

{- | Function invocation payload inside a tool call.

@since 3.0.0.0
-}
data ToolCallFunction = ToolCallFunction'
  { tcfName :: !Text
  , tcfArguments :: !(Map Text Value)
  , tcfIndex :: !(Maybe Int)
  }
  deriving stock (Eq, Show, Generic)

-- | Pattern synonym providing backwards compatibility with 2-argument construction
-- and pattern matching.
--
-- @since 0.4.2.0
pattern ToolCallFunction :: Text -> Map Text Value -> ToolCallFunction
pattern ToolCallFunction name args <- ToolCallFunction' name args _
  where
    ToolCallFunction name args = ToolCallFunction' name args Nothing
{-# COMPLETE ToolCallFunction #-}

-- | Construct a 'ToolCallFunction' with name and arguments map.
--
-- @since 0.4.2.0
mkToolCallFunction :: Text -> Map Text Value -> ToolCallFunction
mkToolCallFunction name args = ToolCallFunction' name args Nothing

instance ToJSON ToolCallFunction where
  toJSON ToolCallFunction' {..} =
    object $
      [ "name" .= tcfName
      , "arguments" .= tcfArguments
      ]
        ++ maybe [] (\i -> ["index" .= i]) tcfIndex

instance FromJSON ToolCallFunction where
  parseJSON = withObject "ToolCallFunction" $ \v ->
    ToolCallFunction'
      <$> v .: "name"
      <*> v .:? "arguments" .!= mempty
      <*> v .:? "index"

-- | Smart constructor for a function parameter representing an object with properties.
--
-- @since 0.4.2.0
objectParams :: [(Text, FunctionParameters)] -> [Text] -> FunctionParameters
objectParams props req =
  FunctionParameters
    { fpType = "object"
    , fpProperties = Just (Map.fromList props)
    , fpRequired = if null req then Nothing else Just req
    , fpAdditionalProperties = Nothing
    , fpDescription = Nothing
    , fpEnum = Nothing
    }

-- | Smart constructor for a string parameter.
--
-- @since 0.4.2.0
stringParam :: Maybe Text -> FunctionParameters
stringParam mdesc =
  FunctionParameters
    { fpType = "string"
    , fpProperties = Nothing
    , fpRequired = Nothing
    , fpAdditionalProperties = Nothing
    , fpDescription = mdesc
    , fpEnum = Nothing
    }

-- | Smart constructor for a number (floating point) parameter.
--
-- @since 0.4.2.0
numberParam :: Maybe Text -> FunctionParameters
numberParam mdesc =
  FunctionParameters
    { fpType = "number"
    , fpProperties = Nothing
    , fpRequired = Nothing
    , fpAdditionalProperties = Nothing
    , fpDescription = mdesc
    , fpEnum = Nothing
    }

-- | Smart constructor for an integer parameter.
--
-- @since 0.4.2.0
intParam :: Maybe Text -> FunctionParameters
intParam mdesc =
  FunctionParameters
    { fpType = "integer"
    , fpProperties = Nothing
    , fpRequired = Nothing
    , fpAdditionalProperties = Nothing
    , fpDescription = mdesc
    , fpEnum = Nothing
    }

-- | Smart constructor for a boolean parameter.
--
-- @since 0.4.2.0
boolParam :: Maybe Text -> FunctionParameters
boolParam mdesc =
  FunctionParameters
    { fpType = "boolean"
    , fpProperties = Nothing
    , fpRequired = Nothing
    , fpAdditionalProperties = Nothing
    , fpDescription = mdesc
    , fpEnum = Nothing
    }

-- | Smart constructor for an enum parameter with predefined string choices.
--
-- @since 0.4.2.0
enumParam :: [Text] -> Maybe Text -> FunctionParameters
enumParam choices mdesc =
  FunctionParameters
    { fpType = "string"
    , fpProperties = Nothing
    , fpRequired = Nothing
    , fpAdditionalProperties = Nothing
    , fpDescription = mdesc
    , fpEnum = Just choices
    }

-- | Smart constructor for a 'FunctionDef'.
--
-- @since 0.4.2.0
functionDef :: Text -> Text -> FunctionParameters -> FunctionDef
functionDef name desc params =
  FunctionDef
    { fnName = name
    , fnDescription = Just desc
    , fnParameters = Just params
    , fnStrict = Just True
    }

-- | Smart constructor for a 'Tool'.
--
-- @since 0.4.2.0
toolDef :: Text -> Text -> FunctionParameters -> Tool
toolDef name desc params =
  Tool
    { toolType = "function"
    , toolFunction = functionDef name desc params
    }
