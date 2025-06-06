{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Data.Ollama.Generate
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Text generation functionality for the Ollama client.

This module provides functions and types for generating text using an Ollama model. It includes APIs
for sending generation requests, both in IO ('generate') and monadic ('generateM') contexts, with
support for streaming and non-streaming responses. The 'GenerateOps' type configures the generation
request, allowing customization of the model, prompt, images, format, and other parameters. The
'defaultGenerateOps' provides a convenient starting point for configuration.

The module supports advanced features like Base64-encoded images, custom templates, and model-specific
options (e.g., temperature). It also includes validation to ensure required fields are non-empty.

Example:

>>> let ops = defaultGenerateOps { modelName = "gemma3", prompt = "Write a poem." }
>>> generate ops Nothing
Right (GenerateResponse ...)
-}
module Data.Ollama.Generate
  ( -- * Generate Texts
    generate
  , generateM

    -- * Configuration
  , defaultGenerateOps
  , GenerateOps (..)
  , validateGenerateOps

    -- * Response and Configuration Types
  , GenerateResponse (..)
  , Format (..)
  , OllamaConfig (..)
  , defaultOllamaConfig
  , ModelOptions (..)
  , defaultModelOptions

    -- * Error Types
  , OllamaError (..)
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Maybe
import Data.Ollama.Common.Config (OllamaConfig (..), defaultOllamaConfig)
import Data.Ollama.Common.Error (OllamaError (..))
import Data.Ollama.Common.Types (Format (..), GenerateResponse (..), ModelOptions (..))
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Text qualified as T

{- | Validates 'GenerateOps' to ensure required fields are non-empty.

Checks that the 'modelName' and 'prompt' fields are not empty. Returns 'Right' with the validated
'GenerateOps' or 'Left' with an 'OllamaError' if validation fails.

Example:

>>> validateGenerateOps defaultGenerateOps
Left (InvalidRequest "Prompt cannot be empty")
--
-- @since 0.2.0.0
-}
validateGenerateOps :: GenerateOps -> Either OllamaError GenerateOps
validateGenerateOps ops
  | T.null (modelName ops) = Left $ InvalidRequest "Model name cannot be empty"
  | T.null (prompt ops) = Left $ InvalidRequest "Prompt cannot be empty"
  | otherwise = Right ops

-- | Configuration for a text generation request.
data GenerateOps = GenerateOps
  { modelName :: !Text
  -- ^ The name of the model to use for generation (e.g., "gemma3").
  , prompt :: !Text
  -- ^ The prompt text to provide to the model for generating a response.
  , suffix :: Maybe Text
  -- ^ Optional suffix to append to the generated text (not supported by all models).
  , images :: !(Maybe [Text])
  -- ^ Optional list of Base64-encoded images to include with the request.
  , format :: !(Maybe Format)
  -- ^ Optional format specifier for the response (e.g., JSON).
  --
  -- @since 0.1.3.0
  , system :: !(Maybe Text)
  -- ^ Optional system text to include in the generation context.
  , template :: !(Maybe Text)
  -- ^ Optional template to format the response.
  , stream :: !(Maybe (GenerateResponse -> IO ()))
  -- ^ Optional callback function to be called with each incoming response.
  , raw :: !(Maybe Bool)
  -- ^ Optional flag to return the raw response.
  , keepAlive :: !(Maybe Int)
  -- ^ Optional override for how long (in minutes) the model stays loaded in memory (default: 5 minutes).
  , options :: !(Maybe ModelOptions)
  -- ^ Optional model parameters (e.g., temperature) as specified in the Modelfile.
  --
  -- @since 0.1.3.0
  , think :: !(Maybe Bool)
  -- ^ Optional flag to enable thinking mode.
  --
  -- @since 0.2.0.0
  }

instance Show GenerateOps where
  show GenerateOps {..} =
    "GenerateOps { "
      <> "model : "
      <> T.unpack modelName
      <> ", prompt : "
      <> T.unpack prompt
      <> ", suffix : "
      <> show suffix
      <> ", images : "
      <> show images
      <> ", format : "
      <> show format
      <> ", system : "
      <> show system
      <> ", template : "
      <> show template
      <> ", stream : "
      <> "Stream functions"
      <> ", raw : "
      <> show raw
      <> ", keepAlive : "
      <> show keepAlive
      <> ", options : "
      <> show options
      <> ", think: "
      <> show think

instance Eq GenerateOps where
  (==) a b =
    modelName a == modelName b
      && prompt a == prompt b
      && suffix a == suffix b
      && images a == images b
      && format a == format b
      && system a == system b
      && template a == template b
      && raw a == raw b
      && keepAlive a == keepAlive b
      && options a == options b
      && think a == think b

instance ToJSON GenerateOps where
  toJSON
    ( GenerateOps
        model
        prompt
        suffix
        images
        format
        system
        template
        stream
        raw
        keepAlive
        options
        think
      ) =
      object
        [ "model" .= model
        , "prompt" .= prompt
        , "suffix" .= suffix
        , "images" .= images
        , "format" .= format
        , "system" .= system
        , "template" .= template
        , "stream" .= if isNothing stream then Just False else Just True
        , "raw" .= raw
        , "keep_alive" .= keepAlive
        , "options" .= options
        , "think" .= think
        ]

{- | Default configuration for text generation.

Provides a default 'GenerateOps' with the "gemma3" model and an empty prompt. Other fields are set
to 'Nothing' or default values. Can be customized by modifying fields as needed.

Example:

>>> let ops = defaultGenerateOps { modelName = "customModel", prompt = "Hello!" }
>>> generate ops Nothing
-}
defaultGenerateOps :: GenerateOps
defaultGenerateOps =
  GenerateOps
    { modelName = "gemma3"
    , prompt = ""
    , suffix = Nothing
    , images = Nothing
    , format = Nothing
    , system = Nothing
    , template = Nothing
    , stream = Nothing
    , raw = Nothing
    , keepAlive = Nothing
    , options = Nothing
    , think = Nothing
    }

{- | Generates text using the specified model and configuration.

Validates the 'GenerateOps' configuration and sends a POST request to the @\/api\/generate@ endpoint.
Supports both streaming and non-streaming responses based on the 'stream' field in 'GenerateOps'.
Returns 'Right' with a 'GenerateResponse' on success or 'Left' with an 'OllamaError' on failure.

Example:

>>> let ops = defaultGenerateOps { modelName = "gemma3", prompt = "Write a short poem." }
>>> generate ops Nothing
Right (GenerateResponse ...)
-}
generate :: GenerateOps -> Maybe OllamaConfig -> IO (Either OllamaError GenerateResponse)
generate ops mbConfig =
  case validateGenerateOps ops of
    Left err -> pure $ Left err
    Right _ -> withOllamaRequest "/api/generate" "POST" (Just ops) mbConfig handler
  where
    handler = case stream ops of
      Nothing -> commonNonStreamingHandler
      Just sendChunk -> commonStreamHandler sendChunk

{- | MonadIO version of 'generate' for use in monadic contexts.

Lifts the 'generate' function into a 'MonadIO' context, allowing it to be used in monadic computations.

Example:

>>> import Control.Monad.IO.Class
>>> let ops = defaultGenerateOps { modelName = "gemma3", prompt = "Hello!" }
>>> runReaderT (generateM ops Nothing) someContext
Right (GenerateResponse ...)
-}
generateM ::
  MonadIO m =>
  GenerateOps -> Maybe OllamaConfig -> m (Either OllamaError GenerateResponse)
generateM ops mbCfg = liftIO $ generate ops mbCfg
