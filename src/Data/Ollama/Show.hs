{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Show
  ( -- * Show Model Info API
    showModel
  , showModelM
  , showModelOpsM
  , showModelOps
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

-- TODO: Add Options parameter

{- |
 #ShowModelOps#
 Input parameters for show model information.
-}
data ShowModelOps = ShowModelOps
  { name :: !Text
  , verbose :: !(Maybe Bool)
  }
  deriving (Show, Eq, Generic, ToJSON)

{- |
 #ShowModelResponse#

 Ouput structure for show model information.
-}
data ShowModelResponse = ShowModelResponse
  { modelFile :: !Text
  , parameters :: !(Maybe Text)
  , template :: !(Maybe Text)
  , details :: !CT.ModelDetails
  , modelInfo :: !ShowModelInfo
  , license :: !(Maybe Text)
  , capabilities :: Maybe [Text]
  }
  deriving (Show, Eq)

data ShowModelInfo = ShowModelInfo
  { generalArchitecture :: !(Maybe Text)
  , generalFileType :: !(Maybe Int)
  , generalParameterCount :: !(Maybe Int64)
  , generalQuantizationVersion :: !(Maybe Int)
  , llamaAttentionHeadCount :: !(Maybe Int)
  , llamaAttentionHeadCountKV :: !(Maybe Int)
  , llamaAttentionLayerNormRMSEpsilon :: !(Maybe Float)
  , llamaBlockCount :: !(Maybe Int)
  , llamaContextLength :: !(Maybe Int)
  , llamaEmbeddingLength :: !(Maybe Int)
  , llamaFeedForwardLength :: !(Maybe Int)
  , llamaRopeDimensionCount :: !(Maybe Int)
  , llamaRopeFreqBase :: !(Maybe Int64)
  , llamaVocabSize :: !(Maybe Int64)
  , tokenizerGgmlBosToken_id :: !(Maybe Int)
  , tokenizerGgmlEosToken_id :: !(Maybe Int)
  , tokenizerGgmlMerges :: !(Maybe [Text])
  , tokenizerGgmlMode :: !(Maybe Text)
  , tokenizerGgmlPre :: !(Maybe Text)
  , tokenizerGgmlTokenType :: !(Maybe [Text])
  , tokenizerGgmlTokens :: !(Maybe [Text])
  }
  deriving (Show, Eq)

-- FromJSON instances

-- | The instance for show model response
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

{- | Show given model's information with options.

@since 1.0.0.0
-}
showModelOps ::
  -- | model name
  Text ->
  -- | verbose
  Maybe Bool ->
  -- | Ollama config
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

{- | Show given model's information.

Higher level API for show.
@since 1.0.0.0
-}
showModel ::
  -- | model name
  Text ->
  IO (Either OllamaError ShowModelResponse)
showModel modelName =
  showModelOps modelName Nothing Nothing

showModelM :: MonadIO m => Text -> m (Either OllamaError ShowModelResponse)
showModelM t = liftIO $ showModel t

showModelOpsM ::
  MonadIO m =>
  Text ->
  Maybe Bool ->
  Maybe OllamaConfig ->
  m (Either OllamaError ShowModelResponse)
showModelOpsM t v mbCfg = liftIO $ showModelOps t v mbCfg
