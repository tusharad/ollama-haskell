{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Embeddings
  ( -- * Embedding API
    embedding
  , embeddingOps
  ) where

import Data.Aeson
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Network.HTTP.Client

-- TODO: Add Options parameter
data EmbeddingOps = EmbeddingOps
  { model :: Text
  , input :: Text
  , truncate :: Maybe Bool
  , keepAlive :: Maybe Text
  }
  deriving (Show, Eq)

data EmbeddingResp = EmbeddingResp
  { model :: Text
  , embedding' :: [[Float]]
  }
  deriving (Show, Eq, Generic, FromJSON)

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
  -- | Model
  Text ->
  -- | Input
  Text ->
  -- | Truncate
  Maybe Bool ->
  -- | Keep Alive
  Maybe Text ->
  IO (Maybe EmbeddingResp)
embeddingOps modelName input_ mTruncate mKeepAlive = do
  let url = defaultOllamaUrl
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest $ T.unpack (url <> "/api/embed")
  let reqBody =
        EmbeddingOps
          { model = modelName
          , input = input_
          , truncate = mTruncate
          , keepAlive = mKeepAlive
          }
      request =
        initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode reqBody
          }
  resp <- httpLbs request manager
  let mRes = decode (responseBody resp) :: Maybe EmbeddingResp
  case mRes of
    Nothing -> pure Nothing
    Just r -> pure $ Just r

-- Higher level binding that only takes important params

-- | Embedding API
embedding ::
  -- | Model
  Text ->
  -- | Input
  Text ->
  IO (Maybe EmbeddingResp)
embedding modelName input_ =
  embeddingOps modelName input_ Nothing Nothing
