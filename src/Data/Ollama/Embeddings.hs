{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Embeddings (embedding,embeddingOps) where

import Data.Aeson
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Client

-- TODO: Add Options parameter
data EmbeddingOps = EmbeddingOps
  { model :: Text,
    input :: Text,
    truncate :: Maybe Bool,
    keepAlive :: Maybe Text
  }
  deriving (Show, Eq)

data EmbeddingResp = EmbeddingResp
  { model :: Text,
    embedding' :: [[Float]]
  }
  deriving (Show, Eq, Generic, FromJSON)

instance ToJSON EmbeddingOps where
  toJSON (EmbeddingOps model input truncate' keepAlive) =
    object
      [ "model" .= model,
        "input" .= input,
        "truncate" .= truncate',
        "keep_alive" .= keepAlive
      ]

embeddingOps ::
  Text ->
  Text ->
  Maybe Bool ->
  Maybe Text ->
  IO (Maybe EmbeddingResp)
embeddingOps modelName input mTruncate mKeepAlive = do
  let url = CU.host defaultOllama
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest $ T.unpack (url <> "/api/embed")
  let reqBody =
        EmbeddingOps
          { model = modelName,
            input = input,
            truncate = mTruncate,
            keepAlive = mKeepAlive
          }
      request =
        initialRequest
          { method = "POST",
            requestBody = RequestBodyLBS $ encode reqBody
          }
  resp <- httpLbs request manager
  let mRes = decode (responseBody resp) :: Maybe EmbeddingResp
  case mRes of
    Nothing -> pure Nothing
    Just r -> pure $ Just r

-- Higher level binding that only takes important params
embedding ::
  Text ->
  Text ->
  IO (Maybe EmbeddingResp)
embedding modelName input =
  embeddingOps modelName input Nothing Nothing
