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
  ToolCall (..),
  ToolCallFunction (..),
) where

import Data.Aeson
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
data ToolCall = ToolCall
  { tcFunction :: !ToolCallFunction
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ToolCall where
  toJSON ToolCall {..} = object ["function" .= tcFunction]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \v ->
    ToolCall <$> v .: "function"

{- | Function invocation payload inside a tool call.

@since 3.0.0.0
-}
data ToolCallFunction = ToolCallFunction
  { tcfName :: !Text
  , tcfArguments :: !(Map Text Value)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ToolCallFunction where
  toJSON ToolCallFunction {..} =
    object
      [ "name" .= tcfName
      , "arguments" .= tcfArguments
      ]

instance FromJSON ToolCallFunction where
  parseJSON = withObject "ToolCallFunction" $ \v ->
    ToolCallFunction
      <$> v .: "name"
      <*> v .: "arguments"
