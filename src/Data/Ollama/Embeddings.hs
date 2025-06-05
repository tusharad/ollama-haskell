{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Embeddings
  ( -- * Embedding API
    embedding
  , embeddingOps
  , embeddingM
  , embeddingOpsM
  , defaultEmbeddingOps
  , defaultModelOptions
  , EmbeddingOps (..)
  , EmbeddingResp (..)
  , ModelOptions (..)
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Ollama.Common.Config (OllamaConfig)
import Data.Ollama.Common.Error (OllamaError)
import Data.Ollama.Common.Types (ModelOptions(..))
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)

defaultEmbeddingOps :: EmbeddingOps
defaultEmbeddingOps =
  EmbeddingOps
    { model = "llama3.2"
    , input = []
    , truncateInput = Nothing
    , keepAliveEmbed = Nothing
    , modelOptions = Nothing
    }

data EmbeddingOps = EmbeddingOps
  { model :: !Text
  , input :: ![Text]
  , truncateInput :: !(Maybe Bool)
  , keepAliveEmbed :: !(Maybe Int)
  , modelOptions :: !(Maybe ModelOptions)
  }
  deriving (Show, Eq)

data EmbeddingResp = EmbeddingResp
  { respondedModel :: !Text
  , respondedEmbeddings :: ![[Float]]
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

-- | Embedding API
embeddingOps ::
  -- | Model
  Text ->
  -- | Input
  [Text] ->
  -- | Truncate
  Maybe Bool ->
  -- | Keep Alive
  Maybe Int ->
  -- | Model options
  Maybe ModelOptions ->
  -- \| Ollama Config
  Maybe
    OllamaConfig ->
  IO (Either OllamaError EmbeddingResp)
embeddingOps modelName input_ mTruncate mKeepAlive mbOptions mbConfig = do
  withOllamaRequest
    "/api/embed"
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

-- Higher level binding that only takes important params

-- | Embedding API
embedding ::
  -- | Model
  Text ->
  -- | Input
  [Text] ->
  IO (Either OllamaError EmbeddingResp)
embedding modelName input_ =
  embeddingOps modelName input_ Nothing Nothing Nothing Nothing

embeddingM :: MonadIO m => Text -> [Text] -> m (Either OllamaError EmbeddingResp)
embeddingM m ip = liftIO $ embedding m ip

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
