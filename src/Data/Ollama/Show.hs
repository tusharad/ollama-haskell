{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Show (
  showModel,showModelOps,ShowModelResponse(..)
) where

import Data.Aeson
import qualified Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.Int (Int64)
import Network.HTTP.Client

-- TODO: Add Options parameter
-- TODO: Add Context parameter
{- |
 #ShowModelOps#
 Input parameters for show model information.
-}
data ShowModelOps = ShowModelOps
  { name :: Text,
    verbose :: Maybe Bool
  }
  deriving (Show, Eq, Generic, ToJSON)

{- |
 #ShowModelResponse#

 Ouput structure for show model information.
-}
data ShowModelResponse = ShowModelResponse
  { modelFile :: Text,
    parameters :: Text,
    template :: Text,
    details :: ModelDetails,
    modelInfo :: ModelInfo
  }
  deriving (Show, Eq)

data ModelDetails = ModelDetails
  { parentModel :: Text,
    format :: Text,
    familiy :: Text,
    families :: [Text],
    parameterSize :: Text,
    quantizationLevel :: Text
  }
  deriving (Show, Eq)

data ModelInfo = ModelInfo
  { generalArchitecture :: Maybe Text,
    generalFileType :: Maybe Int,
    generalParameterCount :: Maybe Int64,
    generalQuantizationVersion :: Maybe Int,
    llamaAttentionHeadCount :: Maybe Int,
    llamaAttentionHeadCountKV :: Maybe Int,
    llamaAttentionLayerNormRMSEpsilon :: Maybe Float,
    llamaBlockCount :: Maybe Int,
    llamaContextLength :: Maybe Int,
    llamaEmbeddingLength :: Maybe Int,
    llamaFeedForwardLength :: Maybe Int,
    llamaRopeDimensionCount :: Maybe Int,
    llamaRopeFreqBase :: Maybe Int64,
    llamaVocabSize :: Maybe Int64,
    tokenizerGgmlBosToken_id :: Maybe Int,
    tokenizerGgmlEosToken_id :: Maybe Int,
    tokenizerGgmlMerges :: Maybe [Text],
    tokenizerGgmlMode :: Maybe Text,
    tokenizerGgmlPre :: Maybe Text,
    tokenizerGgmlTokenType :: Maybe [Text],
    tokenizerGgmlTokens :: Maybe [Text]
  }
  deriving (Show, Eq)

-- FromJSON instances

-- | The instance for show model response
instance FromJSON ShowModelResponse where
  parseJSON = withObject "ShowModelResponse" $ \v ->
    ShowModelResponse
      <$> v .: "modelfile"
      <*> v .: "parameters"
      <*> v .: "template"
      <*> v .: "details"
      <*> v .: "model_info"

instance FromJSON ModelDetails where
  parseJSON = withObject "ModelDetails" $ \v ->
    ModelDetails
      <$> v .: "parent_model"
      <*> v .: "format"
      <*> v .: "family"
      <*> v .: "families"
      <*> v .: "parameter_size"
      <*> v .: "quantization_level"

instance FromJSON ModelInfo where
  parseJSON = withObject "ModelInfo" $ \v ->
    ModelInfo
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

{- | Show given model's information.

@since 1.0.0.0
-}
showModelOps ::
  Text ->
  Maybe Bool ->
  IO (Maybe ShowModelResponse)
showModelOps
  modelName
  verbose =
    do
      let url = CU.host CU.defaultOllama
      manager <- newManager defaultManagerSettings
      initialRequest <- parseRequest $ T.unpack (url <> "/api/show")
      let reqBody =
            ShowModelOps
              { name = modelName,
                verbose = verbose
              }
          request =
            initialRequest
              { method = "POST",
                requestBody = RequestBodyLBS $ encode reqBody
              }
      response <- httpLbs request manager
      let eRes =
            eitherDecode (responseBody response) ::
              Either String ShowModelResponse
      case eRes of
        Left _ -> pure Nothing
        Right r -> pure $ Just r


{- | Show given model's information.

Higher level API for show.
@since 1.0.0.0
-}
showModel :: 
  Text -> 
  IO (Maybe ShowModelResponse)
showModel modelName =
  showModelOps modelName Nothing
