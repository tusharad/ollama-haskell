{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Embeddings
  ( -- * Embedding API
    embedding
  , embeddingOps
  , defaultEmbeddingOps
  , EmbeddingOps (..)
  , EmbeddingResp (..)
  ) where

import Data.Aeson
import Data.Ollama.Common.Config (OllamaConfig)
import Data.Ollama.Common.Error (OllamaError)
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Ollama.Common.Types (ModelOptions)

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
  { respondedModeel :: !Text
  , embedding_ :: ![[Float]]
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

-- TODO: Add Options parameter

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
  -- | Ollama Config
  Maybe ModelOptions ->
  -- | Model options
  Maybe OllamaConfig ->
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
