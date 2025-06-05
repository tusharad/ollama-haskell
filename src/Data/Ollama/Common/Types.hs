{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Data.Ollama.Common.Types
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Shared data types for request and response structures used throughout the Ollama client.

== 📋 Overview

This module defines common types for working with Ollama's API, including:

- Chat messages and roles
- Text generation responses
- Structured function/tool calling
- Model metadata
- Streaming handling
- Custom model parameters

These types are consumed and returned by higher-level modules
like `Data.Ollama.Chat`, `Data.Ollama.Generate`, and others.

== Includes

- Chat message structure and roles
- Generate and chat response records
- ModelOptions and advanced config
- Structured function/tool call interfaces
- JSON format hints and schema wrapping
- Helper class 'HasDone' for streaming termination

Most types implement `ToJSON`/`FromJSON` for direct API interaction.
-}
module Data.Ollama.Common.Types
  ( ModelDetails (..)
  , Format (..)
  , GenerateResponse (..)
  , Message (..)
  , Role (..)
  , ChatResponse (..)
  , HasDone (..)
  , ModelOptions (..)
  , InputTool (..)
  , FunctionDef (..)
  , FunctionParameters (..)
  , ToolCall (..)
  , OutputFunction (..)
  , Version (..)
  ) where

import Data.Aeson
import Data.Map qualified as HM
import Data.Maybe (catMaybes)
import Data.Ollama.Common.SchemaBuilder
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import GHC.Int (Int64)

-- | Metadata describing a specific model's identity and configuration.
data ModelDetails = ModelDetails
  { parentModel :: !(Maybe Text)
  -- ^ The parent model from which this model was derived, if any.
  , format :: !Text
  -- ^ The format used for the model (e.g., "gguf").
  , family :: !Text
  -- ^ The family name of the model (e.g., "llama", "mistral").
  , families :: ![Text]
  -- ^ Alternative or related family identifiers.
  , parameterSize :: !Text
  -- ^ The size of the model's parameters, typically expressed as a string (e.g., "7B").
  , quantizationLevel :: Text
  -- ^ The quantization level used (e.g., "Q4", "Q8").
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

{- | Format specification for the chat output.

@since 0.1.3.0
-}
data Format = JsonFormat | SchemaFormat Schema
  deriving (Show, Eq)

instance ToJSON Format where
  toJSON JsonFormat = String "json"
  toJSON (SchemaFormat schema) = toJSON schema

{- |
Result type for 'generate' function containing the model's response and meta-information.
-}
data GenerateResponse = GenerateResponse
  { model :: !Text
  -- ^ The name of the model that generated the response.
  , createdAt :: !UTCTime
  -- ^ The timestamp when the response was created.
  , genResponse :: !Text
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
  , thinking :: !(Maybe Text)
  -- ^ Thinking of reasoning models; if think is set to true
  --
  -- @since 0.2.0.0
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
      <*> v .:? "thinking"

-- | Enumerated roles that can participate in a chat.
data Role = System | User | Assistant | Tool
  deriving (Show, Eq)

instance ToJSON Role where
  toJSON System = String "system"
  toJSON User = String "user"
  toJSON Assistant = String "assistant"
  toJSON Tool = String "tool"

instance FromJSON Role where
  parseJSON = withText "Role" $ \t ->
    case t of
      "system" -> pure System
      "user" -> pure User
      "assistant" -> pure Assistant
      "tool" -> pure Tool
      _ -> fail $ "Invalid Role value: " <> show t

-- | Represents a message within a chat, including its role and content.
data Message = Message
  { role :: !Role
  -- ^ The role of the entity sending the message (e.g., 'User', 'Assistant').
  , content :: !Text
  -- ^ The textual content of the message.
  , images :: !(Maybe [Text])
  -- ^ Optional list of base64 encoded images that accompany the message.
  , tool_calls :: !(Maybe [ToolCall])
  -- ^ a list of tools in JSON that the model wants to use
  --
  -- @since 0.1.3.0
  , thinking :: !(Maybe Text)
  --
  -- @since 0.2.0.0
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
  getDone GenerateResponse {..} = done

instance HasDone ChatResponse where
  getDone ChatResponse {..} = done

-- | Optional model tuning parameters that influence generation behavior.
--
-- @since 0.2.0.0
data ModelOptions = ModelOptions
  { numKeep :: Maybe Int
  , -- \^ Number of tokens to keep from the previous context.
    seed :: Maybe Int
  , -- \^ Random seed for reproducibility.
    numPredict :: Maybe Int
  , -- \^ Maximum number of tokens to predict.
    topK :: Maybe Int
  , -- \^ Top-K sampling parameter.
    topP :: Maybe Double
  , -- \^ Top-P (nucleus) sampling parameter.
    minP :: Maybe Double
  , -- \^ Minimum probability for nucleus sampling.
    typicalP :: Maybe Double
  , -- \^ Typical sampling probability.
    repeatLastN :: Maybe Int
  , -- \^ Number of tokens to consider for repetition penalty.
    temperature :: Maybe Double
  , -- \^ Sampling temperature. Higher = more randomness.
    repeatPenalty :: Maybe Double
  , -- \^ Penalty for repeating the same tokens.
    presencePenalty :: Maybe Double
  , -- \^ Penalty for introducing new tokens.
    frequencyPenalty :: Maybe Double
  , -- \^ Penalty for frequent tokens.
    penalizeNewline :: Maybe Bool
  , -- \^ Whether to penalize newline tokens.
    stop :: Maybe [Text]
  , -- \^ List of stop sequences to end generation.
    numa :: Maybe Bool
  , -- \^ Whether to enable NUMA-aware optimizations.
    numCtx :: Maybe Int
  , -- \^ Number of context tokens.
    numBatch :: Maybe Int
  , -- \^ Batch size used during generation.
    numGpu :: Maybe Int
  , -- \^ Number of GPUs to use.
    mainGpu :: Maybe Int
  , -- \^ Index of the primary GPU to use.
    useMmap :: Maybe Bool
  , -- \^ Whether to memory-map the model.
    numThread :: Maybe Int
  }
  -- \^ Number of threads to use for inference.

  deriving (Show, Eq)

-- | Custom ToJSON instance for Options
instance ToJSON ModelOptions where
  toJSON opts =
    object $
      catMaybes
        [ ("num_keep" .=) <$> numKeep opts
        , ("seed" .=) <$> seed opts
        , ("num_predict" .=) <$> numPredict opts
        , ("top_k" .=) <$> topK opts
        , ("top_p" .=) <$> topP opts
        , ("min_p" .=) <$> minP opts
        , ("typical_p" .=) <$> typicalP opts
        , ("repeat_last_n" .=) <$> repeatLastN opts
        , ("temperature" .=) <$> temperature opts
        , ("repeat_penalty" .=) <$> repeatPenalty opts
        , ("presence_penalty" .=) <$> presencePenalty opts
        , ("frequency_penalty" .=) <$> frequencyPenalty opts
        , ("penalize_newline" .=) <$> penalizeNewline opts
        , ("stop" .=) <$> stop opts
        , ("numa" .=) <$> numa opts
        , ("num_ctx" .=) <$> numCtx opts
        , ("num_batch" .=) <$> numBatch opts
        , ("num_gpu" .=) <$> numGpu opts
        , ("main_gpu" .=) <$> mainGpu opts
        , ("use_mmap" .=) <$> useMmap opts
        , ("num_thread" .=) <$> numThread opts
        ]

-- | A wrapper for the Ollama engine version string.
newtype Version = Version Text
  deriving (Eq, Show)

instance FromJSON Version where
  parseJSON = withObject "version" $ \v -> do
    Version <$> v .: "version"

-- | Represents a tool that can be used in the conversation.
--
-- @since 0.2.0.0
data InputTool = InputTool
  { toolType :: Text
  -- ^ The type of the tool
  , function :: FunctionDef
  -- ^ The function associated with the tool
  }
  deriving (Show, Eq, Generic)

instance ToJSON InputTool where
  toJSON InputTool {..} =
    object
      [ "type" .= toolType
      , "function" .= function
      ]

instance FromJSON InputTool where
  parseJSON = withObject "Tool" $ \v ->
    InputTool
      <$> v .: "type"
      <*> v .: "function"

-- | Represents a function that can be called by the model.
--
-- @since 0.2.0.0
data FunctionDef = FunctionDef
  { functionName :: Text
  -- ^ The name of the function
  , functionDescription :: Maybe Text
  -- ^ Optional description of the function
  , functionParameters :: Maybe FunctionParameters
  -- ^ Optional parameters for the function
  , functionStrict :: Maybe Bool
  -- ^ Optional strictness flag
  }
  deriving (Show, Eq, Generic)

instance ToJSON FunctionDef where
  toJSON FunctionDef {..} =
    object $
      [ "name" .= functionName
      ]
        ++ maybe [] (\d -> ["description" .= d]) functionDescription
        ++ maybe [] (\p -> ["parameters" .= p]) functionParameters
        ++ maybe [] (\s -> ["strict" .= s]) functionStrict

instance FromJSON FunctionDef where
  parseJSON = withObject "Function" $ \v ->
    FunctionDef
      <$> v .: "name"
      <*> v .:? "description"
      <*> v .:? "parameters"
      <*> v .:? "strict"

-- | Parameters definition for a function call used in structured output or tool calls.
--
-- @since 0.2.0.0
data FunctionParameters = FunctionParameters
  { parameterType :: Text
  -- ^ Type of the parameter (usually "object").
  , parameterProperties :: Maybe (HM.Map Text FunctionParameters)
  -- ^ Optional nested parameters as a property map.
  , requiredParams :: Maybe [Text]
  -- ^ List of required parameter names.
  , additionalProperties :: Maybe Bool
  -- ^ Whether additional (unspecified) parameters are allowed.
  }
  deriving (Show, Eq)

instance ToJSON FunctionParameters where
  toJSON FunctionParameters {..} =
    object
      [ "type" .= parameterType
      , "properties" .= parameterProperties
      , "required" .= requiredParams
      , "additionalProperties" .= additionalProperties
      ]

instance FromJSON FunctionParameters where
  parseJSON = withObject "parameters" $ \v ->
    FunctionParameters
      <$> v .: "type"
      <*> v .: "properties"
      <*> v .: "required"
      <*> v .: "additionalProperties"

-- | A single tool call returned from the model, containing the function to be invoked.
--
-- @since 0.2.0.0
newtype ToolCall = ToolCall
  { outputFunction :: OutputFunction
  -- ^ The function the model intends to call, with arguments.
  }
  deriving (Show, Eq)

-- | Output representation of a function to be called, including its name and arguments.
--
-- @since 0.2.0.0
data OutputFunction = OutputFunction
  { outputFunctionName :: Text
  -- ^ The name of the function to invoke.
  , arguments :: HM.Map Text Value
  -- ^ A key-value map of argument names to values (JSON values).
  }
  deriving (Eq, Show)

instance ToJSON OutputFunction where
  toJSON OutputFunction {..} =
    object
      [ "name" .= outputFunctionName
      , "arguments" .= arguments
      ]

instance FromJSON OutputFunction where
  parseJSON = withObject "function" $ \v ->
    OutputFunction
      <$> v .: "name"
      <*> v .: "arguments"

instance ToJSON ToolCall where
  toJSON ToolCall {..} = object ["function" .= outputFunction]

instance FromJSON ToolCall where
  parseJSON = withObject "tool_calls" $ \v ->
    ToolCall <$> v .: "function"
