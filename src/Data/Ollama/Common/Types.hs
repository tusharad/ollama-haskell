{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Ollama.Common.Types
  ( ModelDetails (..)
  , OllamaClient (..)
  , Format (..)
  , GenerateResponse (..)
  , Message (..)
  , Role (..)
  , ChatResponse (..)
  , HasDone (..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Int (Int64)
import GHC.Generics

data ModelDetails = ModelDetails
  { parentModel :: !(Maybe Text)
  , format :: !Text
  , familiy :: !Text
  , families :: ![Text]
  , parameterSize :: !Text
  , quantizationLevel :: Text
  }
  deriving (Eq, Show)

instance FromJSON ModelDetails where
  parseJSON = withObject "ModelDetails" $ \v ->
    ModelDetails
      <$> v .: "parent_model"
      <*> v .: "format"
      <*> v .: "family"
      <*> v .:? "families" .!= []
      <*> v .: "parameter_size"
      <*> v .: "quantization_level"

newtype OllamaClient = OllamaClient
  { host :: Text
  }
  deriving (Eq, Show)


{-|
E.g SchemaFormat
{
    "type": "object",
    "properties": {
      "age": {
        "type": "integer"
      },
      "available": {
        "type": "boolean"
      }
    },
    "required": [
      "age",
      "available"
    ]
  }
|-}
-- | Format specification for the chat output
-- | Since 0.1.3.0
data Format = JsonFormat | SchemaFormat Value
  deriving (Show, Eq)

instance ToJSON Format where
  toJSON JsonFormat = String "json"
  toJSON (SchemaFormat schema) = schema

-- TODO: Add Context Param
{- |
Result type for generate function containing the model's response and meta-information.
-}
data GenerateResponse = GenerateResponse
  { model :: !Text
  -- ^ The name of the model that generated the response.
  , createdAt :: !UTCTime
  -- ^ The timestamp when the response was created.
  , response_ :: !Text
  -- ^ The generated response from the model.
  , done :: !Bool
  -- ^ A flag indicating whether the generation process is complete.
  , totalDuration :: !(Maybe Int64)
  -- ^ Optional total duration in milliseconds for the generation process.
  , loadDuration :: !(Maybe Int64)
  -- ^ Optional load duration in milliseconds for loading the model.
  , promptEvalCount :: !(Maybe Int64)
  -- ^ Optional count of prompt evaluations during the generation process.
  , promptEvalDuration :: !(Maybe Int64)
  -- ^ Optional duration in milliseconds for evaluating the prompt.
  , evalCount :: !(Maybe Int64)
  -- ^ Optional count of evaluations during the generation process.
  , evalDuration :: !(Maybe Int64)
  -- ^ Optional duration in milliseconds for evaluations during the generation process.
  }
  deriving (Show, Eq)

instance FromJSON GenerateResponse where
  parseJSON = withObject "GenerateResponse" $ \v ->
    GenerateResponse
      <$> v .: "model"
      <*> v .: "created_at"
      <*> v .: "response"
      <*> v .: "done"
      <*> v .:? "total_duration"
      <*> v .:? "load_duration"
      <*> v .:? "prompt_eval_count"
      <*> v .:? "prompt_eval_duration"
      <*> v .:? "eval_count"
      <*> v .:? "eval_duration"

-- | Enumerated roles that can participate in a chat.
data Role = System | User | Assistant | Tool
  deriving (Show, Eq)

instance ToJSON Role where
  toJSON System = String "system"
  toJSON User = String "user"
  toJSON Assistant = String "assistant"
  toJSON Tool = String "tool"

instance FromJSON Role where
  parseJSON (String "system") = pure System
  parseJSON (String "user") = pure User
  parseJSON (String "assistant") = pure Assistant
  parseJSON (String "tool") = pure Tool
  parseJSON _ = fail "Invalid Role value"

-- TODO : Add tool_calls parameter

-- | Represents a message within a chat, including its role and content.
data Message = Message
  { role :: !Role
  -- ^ The role of the entity sending the message (e.g., 'User', 'Assistant').
  , content :: !Text
  -- ^ The textual content of the message.
  , images :: !(Maybe [Text])
  -- ^ Optional list of base64 encoded images that accompany the message.
  , tool_calls :: !(Maybe [Value])
  -- ^ a list of tools in JSON that the model wants to use
  -- ^ Since 0.1.3.0
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ChatResponse = ChatResponse
  { model :: !Text
  -- ^ The name of the model that generated this response.
  , createdAt :: !UTCTime
  -- ^ The timestamp when the response was created.
  , message :: !(Maybe Message)
  -- ^ The message content of the response, if any.
  , done :: !Bool
  -- ^ Indicates whether the chat process has completed.
  , totalDuration :: !(Maybe Int64)
  -- ^ Optional total duration in milliseconds for the chat process.
  , loadDuration :: !(Maybe Int64)
  -- ^ Optional load duration in milliseconds for loading the model.
  , promptEvalCount :: !(Maybe Int64)
  -- ^ Optional count of prompt evaluations during the chat process.
  , promptEvalDuration :: !(Maybe Int64)
  -- ^ Optional duration in milliseconds for evaluating the prompt.
  , evalCount :: !(Maybe Int64)
  -- ^ Optional count of evaluations during the chat process.
  , evalDuration :: !(Maybe Int64)
  -- ^ Optional duration in milliseconds for evaluations during the chat process.
  }
  deriving (Show, Eq)

instance FromJSON ChatResponse where
  parseJSON = withObject "ChatResponse" $ \v ->
    ChatResponse
      <$> v .: "model"
      <*> v .: "created_at"
      <*> v .: "message"
      <*> v .: "done"
      <*> v .:? "total_duration"
      <*> v .:? "load_duration"
      <*> v .:? "prompt_eval_count"
      <*> v .:? "prompt_eval_duration"
      <*> v .:? "eval_count"
      <*> v .:? "eval_duration"

-- | A workaround to use done field within commonStreamHandler
class HasDone a where
  getDone :: a -> Bool

instance HasDone GenerateResponse where
  getDone GenerateResponse{..} = done

instance HasDone ChatResponse where
  getDone ChatResponse{..} = done
