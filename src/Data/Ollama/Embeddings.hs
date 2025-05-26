{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Embeddings
  ( -- * Embedding API
    embedding
  , embeddingOps
  , EmbeddingOps (..)
  , EmbeddingResp (..)
  ) where

import Data.Aeson
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)

-- TODO: Add Options parameter
data EmbeddingOps = EmbeddingOps
  { model :: !Text
  , input :: !Text
  , truncate :: !(Maybe Bool)
  , keepAlive :: !(Maybe Text)
  }
  deriving (Show, Eq)

data EmbeddingResp = EmbeddingResp
  { model :: !Text
  , embedding_ :: ![[Float]]
  }
  deriving (Show, Eq)

instance FromJSON EmbeddingResp where
  parseJSON = withObject "EmbeddingResp" $ \v ->
    EmbeddingResp
      <$> v .: "model"
      <*> v .: "embeddings"

instance ToJSON EmbeddingOps where
  toJSON (EmbeddingOps model_ input_ truncate' keepAlive_) =
    object
      [ "model" .= model_
      , "input" .= input_
      , "truncate" .= truncate'
      , "keep_alive" .= keepAlive_
      ]

-- TODO: Add Options parameter

-- | Embedding API
embeddingOps ::
  -- | Ollama URL
  Maybe Text ->
  -- | Model
  Text ->
  -- | Input
  Text ->
  -- | Truncate
  Maybe Bool ->
  -- | Keep Alive
  Maybe Text ->
  IO (Either String EmbeddingResp)
embeddingOps hostUrl modelName input_ mTruncate mKeepAlive = do
  withOllamaRequest
    "/api/embed"
    "POST"
    ( Just $
        EmbeddingOps
          { model = modelName
          , input = input_
          , truncate = mTruncate
          , keepAlive = mKeepAlive
          }
    )
    hostUrl
    Nothing
    commonNonStreamingHandler

-- Higher level binding that only takes important params
-- | Embedding API
embedding ::
  -- | Model
  Text ->
  -- | Input
  Text ->
  IO (Either String EmbeddingResp)
embedding modelName input_ =
  embeddingOps Nothing modelName input_ Nothing Nothing
