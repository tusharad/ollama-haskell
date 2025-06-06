{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Ollama.Show
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Functionality for retrieving detailed information about models in the Ollama client.

This module provides functions to fetch detailed information about a specific model on the Ollama server.
It includes both high-level ('showModel', 'showModelM') and low-level ('showModelOps', 'showModelOpsM') APIs
for retrieving model details, with support for verbose output. The operation is performed via a POST request
to the @\/api\/show@ endpoint, returning a 'ShowModelResponse' containing comprehensive model metadata.

The 'ShowModelOps' type configures the request, and 'ShowModelResponse' and 'ShowModelInfo' represent the
response structure. The module also re-exports 'CT.ModelDetails' for completeness.

Note: Verbose mode parsing is currently not fully supported.

Example:

>>> showModel "gemma3"
Right (ShowModelResponse {modelFile = "...", ...})

@since 1.0.0.0
-}
module Data.Ollama.Show
  ( -- * Show Model Info API
    showModel
  , showModelM
  , showModelOps
  , showModelOpsM

    -- * Response Types
  , ShowModelResponse (..)
  , ShowModelInfo (..)
  , CT.ModelDetails (..)
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Ollama.Common.Config (OllamaConfig)
import Data.Ollama.Common.Error (OllamaError)
import Data.Ollama.Common.Types qualified as CT
import Data.Ollama.Common.Utils (commonNonStreamingHandler, withOllamaRequest)
import Data.Text (Text)
import GHC.Generics
import GHC.Int (Int64)

-- | Configuration options for requesting model information.
data ShowModelOps = ShowModelOps
  { name :: !Text
  -- ^ The name of the model to query (e.g., "gemma3").
  , verbose :: !(Maybe Bool)
  -- ^ Optional flag to request verbose output. Note: Verbose mode parsing is currently incomplete.
  }
  deriving (Show, Eq, Generic, ToJSON)

-- | Response structure for model information.
data ShowModelResponse = ShowModelResponse
  { modelFile :: !Text
  -- ^ The content of the model's Modelfile.
  , parameters :: !(Maybe Text)
  -- ^ Optional model parameters (e.g., temperature settings).
  , template :: !(Maybe Text)
  -- ^ Optional template used for the model.
  , details :: !CT.ModelDetails
  -- ^ General details about the model (e.g., format, family).
  , modelInfo :: !ShowModelInfo
  -- ^ Detailed technical information about the model.
  , license :: !(Maybe Text)
  -- ^ Optional license information for the model.
  --
  -- @since 0.2.0.0
  , capabilities :: Maybe [Text]
  -- ^ Optional list of model capabilities.
  --
  -- @since 0.2.0.0
  }
  deriving (Show, Eq)

-- | Detailed technical information about a model.
data ShowModelInfo = ShowModelInfo
  { generalArchitecture :: !(Maybe Text)
  -- ^ The architecture of the model (e.g., "llama").
  , generalFileType :: !(Maybe Int)
  -- ^ The file type identifier for the model.
  , generalParameterCount :: !(Maybe Int64)
  -- ^ The number of parameters in the model.
  , generalQuantizationVersion :: !(Maybe Int)
  -- ^ The quantization version used by the model.
  , llamaAttentionHeadCount :: !(Maybe Int)
  -- ^ Number of attention heads in the LLaMA model.
  , llamaAttentionHeadCountKV :: !(Maybe Int)
  -- ^ Number of key-value attention heads in the LLaMA model.
  , llamaAttentionLayerNormRMSEpsilon :: !(Maybe Float)
  -- ^ RMS epsilon for layer normalization in the LLaMA model.
  , llamaBlockCount :: !(Maybe Int)
  -- ^ Number of blocks in the LLaMA model.
  , llamaContextLength :: !(Maybe Int)
  -- ^ Context length supported by the LLaMA model.
  , llamaEmbeddingLength :: !(Maybe Int)
  -- ^ Embedding length used by the LLaMA model.
  , llamaFeedForwardLength :: !(Maybe Int)
  -- ^ Feed-forward layer length in the LLaMA model.
  , llamaRopeDimensionCount :: !(Maybe Int)
  -- ^ RoPE dimension count in the LLaMA model.
  , llamaRopeFreqBase :: !(Maybe Int64)
  -- ^ Base frequency for RoPE in the LLaMA model.
  , llamaVocabSize :: !(Maybe Int64)
  -- ^ Vocabulary size of the LLaMA model.
  , tokenizerGgmlBosToken_id :: !(Maybe Int)
  -- ^ BOS (beginning of sequence) token ID for the GGML tokenizer.
  , tokenizerGgmlEosToken_id :: !(Maybe Int)
  -- ^ EOS (end of sequence) token ID for the GGML tokenizer.
  , tokenizerGgmlMerges :: !(Maybe [Text])
  -- ^ List of merges for the GGML tokenizer.
  , tokenizerGgmlMode :: !(Maybe Text)
  -- ^ Mode of the GGML tokenizer.
  , tokenizerGgmlPre :: !(Maybe Text)
  -- ^ Pre-tokenization configuration for the GGML tokenizer.
  , tokenizerGgmlTokenType :: !(Maybe [Text])
  -- ^ Token type information for the GGML tokenizer.
  , tokenizerGgmlTokens :: !(Maybe [Text])
  -- ^ List of tokens for the GGML tokenizer.
  }
  deriving (Show, Eq)

-- | JSON parsing instance for 'ShowModelResponse'.
instance FromJSON ShowModelResponse where
  parseJSON = withObject "ShowModelResponse" $ \v ->
    ShowModelResponse
      <$> v .: "modelfile"
      <*> v .:? "parameters"
      <*> v .:? "template"
      <*> v .: "details"
      <*> v .: "model_info"
      <*> v .:? "license"
      <*> v .:? "capabilities"

-- | JSON parsing instance for 'ShowModelInfo'.
instance FromJSON ShowModelInfo where
  parseJSON = withObject "ModelInfo" $ \v ->
    ShowModelInfo
      <$> v .:? "general.architecture"
      <*> v .:? "general.file_type"
      <*> v .:? "general.parameter_count"
      <*> v .:? "general.quantization_version"
      <*> v .:? "llama.attention.head_count"
      <*> v .:? "llama.attention.head_count_kv"
      <*> v .:? "llama.attention.layer_norm_rms_epsilon"
      <*> v .:? "llama.block_count"
      <*> v .:? "llama.context_length"
      <*> v .:? "llama.embedding_length"
      <*> v .:? "llama.feed_forward_length"
      <*> v .:? "llama.rope.dimension_count"
      <*> v .:? "llama.rope.freq_base"
      <*> v .:? "llama.vocab_size"
      <*> v .:? "tokenizer.ggml.bos_token_id"
      <*> v .:? "tokenizer.ggml.eos_token_id"
      <*> v .:? "tokenizer.ggml.merges"
      <*> v .:? "tokenizer.ggml.model"
      <*> v .:? "tokenizer.ggml.pre"
      <*> v .:? "tokenizer.ggml.token_type"
      <*> v .:? "tokenizer.ggml.tokens"

{- | Retrieves model information with configuration options.

Sends a POST request to the @\/api\/show@ endpoint to fetch detailed information about
the specified model. Supports verbose output if 'verbose' is 'Just True' (though verbose
mode parsing is currently incomplete). Returns 'Right' with a 'ShowModelResponse' on
success or 'Left' with an 'OllamaError' on failure.
-}
showModelOps ::
  -- | Model name
  Text ->
  -- | Optional verbose flag
  Maybe Bool ->
  -- | Optional 'OllamaConfig' (defaults to 'defaultOllamaConfig' if 'Nothing')
  Maybe OllamaConfig ->
  IO (Either OllamaError ShowModelResponse)
showModelOps modelName verbose_ mbConfig = do
  withOllamaRequest
    "/api/show"
    "POST"
    ( Just $
        ShowModelOps
          { name = modelName
          , verbose = verbose_
          }
    )
    mbConfig
    commonNonStreamingHandler

{- | Simplified API for retrieving model information.

A higher-level function that fetches model information using default settings for
verbose output and Ollama configuration. Suitable for basic use cases.
-}
showModel ::
  -- | Model name
  Text ->
  IO (Either OllamaError ShowModelResponse)
showModel modelName =
  showModelOps modelName Nothing Nothing

{- | MonadIO version of 'showModel' for use in monadic contexts.

Lifts the 'showModel' function into a 'MonadIO' context, allowing it to be used in
monadic computations.
-}
showModelM :: MonadIO m => Text -> m (Either OllamaError ShowModelResponse)
showModelM t = liftIO $ showModel t

{- | MonadIO version of 'showModelOps' for use in monadic contexts.

Lifts the 'showModelOps' function into a 'MonadIO' context, allowing it to be used in
monadic computations with full configuration options.
-}
showModelOpsM ::
  MonadIO m =>
  Text ->
  Maybe Bool ->
  Maybe OllamaConfig ->
  m (Either OllamaError ShowModelResponse)
showModelOpsM t v mbCfg = liftIO $ showModelOps t v mbCfg
