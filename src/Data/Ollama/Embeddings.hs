{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Ollama.Embeddings
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Functionality for generating text embeddings using the Ollama API.

This module provides functions to generate text embeddings from an Ollama model. It includes both
high-level ('embedding', 'embeddingM') and low-level ('embeddingOps', 'embeddingOpsM') APIs for
generating embeddings, with support for customizing model options, truncation, and keep-alive settings.
The embeddings are returned as a list of float vectors, suitable for tasks like semantic search or
text similarity analysis.

The 'EmbeddingOps' type configures the embedding request, and 'EmbeddingResp' represents the response
containing the model name and the generated embeddings. The 'defaultEmbeddingOps' provides a default
configuration for convenience.

Example:

>>> embedding "llama3.2" ["Hello, world!"]
Right (EmbeddingResp "llama3.2" [[0.1, 0.2, ...]])
-}
module Data.Ollama.Embeddings
  ( -- * Embedding API
    embedding
  , embeddingOps
  , embeddingM
  , embeddingOpsM

    -- * Configuration and Response Types
  , defaultEmbeddingOps
  , EmbeddingOps (..)
  , EmbeddingResp (..)

    -- * Model Options
  , ModelOptions (..)
  , defaultModelOptions
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Ollama.Common.Config (OllamaConfig)
import Data.Ollama.Common.Error (OllamaError)
import Data.Ollama.Common.Types (ModelOptions (..))
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)

{- | Default configuration for embedding requests.

Provides a default 'EmbeddingOps' with the "llama3.2" model, an empty input list, and no additional options.
Can be customized by modifying fields as needed.
-}
defaultEmbeddingOps :: EmbeddingOps
defaultEmbeddingOps =
  EmbeddingOps
    { model = "llama3.2"
    , input = []
    , truncateInput = Nothing
    , keepAliveEmbed = Nothing
    , modelOptions = Nothing
    }

-- | Configuration for an embedding request.
data EmbeddingOps = EmbeddingOps
  { model :: !Text
  -- ^ The name of the model to use for generating embeddings (e.g., "llama3.2").
  , input :: ![Text]
  -- ^ List of input texts to generate embeddings for.
  , truncateInput :: !(Maybe Bool)
  -- ^ Optional flag to truncate input if it exceeds model limits.
  , keepAliveEmbed :: !(Maybe Int)
  -- ^ Optional override for the keep-alive timeout in minutes.
  , modelOptions :: !(Maybe ModelOptions)
  -- ^ Optional model parameters (e.g., temperature) as specified in the Modelfile.
  --
  -- @since 0.2.0.0
  }
  deriving (Show, Eq)

-- | Response type for an embedding request.
data EmbeddingResp = EmbeddingResp
  { respondedModel :: !Text
  -- ^ The name of the model that generated the embeddings.
  , respondedEmbeddings :: ![[Float]]
  -- ^ List of embedding vectors, one for each input text.
  }
  deriving (Show, Eq)

instance FromJSON EmbeddingResp where
  parseJSON = withObject "EmbeddingResp" $ \v ->
    EmbeddingResp
      <$> v .: "model"
      <*> v .: "embeddings"

instance ToJSON EmbeddingOps where
  toJSON (EmbeddingOps model_ input_ truncate' keepAlive_ ops) =
    object
      [ "model" .= model_
      , "input" .= input_
      , "truncate" .= truncate'
      , "keep_alive" .= keepAlive_
      , "options" .= ops
      ]

{- | Generates embeddings for a list of input texts with full configuration.

Sends a POST request to the "/api//embed" endpoint to generate embeddings for the provided inputs.
Allows customization of truncation, keep-alive settings, model options, and Ollama configuration.
Returns 'Right' with an 'EmbeddingResp' on success or 'Left' with an 'OllamaError' on failure.
-}
embeddingOps ::
  -- | Model name
  Text ->
  -- | List of input texts
  [Text] ->
  -- | Optional truncation flag
  Maybe Bool ->
  -- | Optional keep-alive timeout in minutes
  Maybe Int ->
  -- | Optional model options
  Maybe ModelOptions ->
  -- | Optional 'OllamaConfig' (defaults to 'defaultOllamaConfig' if 'Nothing')
  Maybe OllamaConfig ->
  IO (Either OllamaError EmbeddingResp)
embeddingOps modelName input_ mTruncate mKeepAlive mbOptions mbConfig = do
  withOllamaRequest
    "/api//embed"
    "POST"
    ( Just $
        EmbeddingOps
          { model = modelName
          , input = input_
          , truncateInput = mTruncate
          , keepAliveEmbed = mKeepAlive
          , modelOptions = mbOptions
          }
    )
    mbConfig
    commonNonStreamingHandler

{- | Simplified API for generating embeddings.

A higher-level function that generates embeddings using default settings for truncation, keep-alive,
model options, and Ollama configuration. Suitable for basic use cases.
-}
embedding ::
  -- | Model name
  Text ->
  -- | List of input texts
  [Text] ->
  IO (Either OllamaError EmbeddingResp)
embedding modelName input_ =
  embeddingOps modelName input_ Nothing Nothing Nothing Nothing

{- | MonadIO version of 'embedding' for use in monadic contexts.

Lifts the 'embedding' function into a 'MonadIO' context, allowing it to be used in monadic computations.
-}
embeddingM :: MonadIO m => Text -> [Text] -> m (Either OllamaError EmbeddingResp)
embeddingM m ip = liftIO $ embedding m ip

{- | MonadIO version of 'embeddingOps' for use in monadic contexts.

Lifts the 'embeddingOps' function into a 'MonadIO' context, allowing it to be used in monadic computations
with full configuration options.
-}
embeddingOpsM ::
  MonadIO m =>
  Text ->
  [Text] ->
  Maybe Bool ->
  Maybe Int ->
  Maybe ModelOptions ->
  Maybe OllamaConfig ->
  m (Either OllamaError EmbeddingResp)
embeddingOpsM m ip mbTruncate mbKeepAlive mbOptions mbCfg =
  liftIO $ embeddingOps m ip mbTruncate mbKeepAlive mbOptions mbCfg
