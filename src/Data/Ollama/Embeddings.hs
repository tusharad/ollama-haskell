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
import Data.Text qualified as T
import Data.Maybe (fromMaybe)
import Network.HTTP.Client
import Control.Exception (try)
import Data.ByteString.Lazy.Char8 (ByteString)

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
  , embedding_ :: [[Float]]
  }
  deriving (Show, Eq)

instance FromJSON EmbeddingResp where
  parseJSON = withObject "EmbeddingResp" $ \v -> EmbeddingResp
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
  let url = fromMaybe defaultOllamaUrl hostUrl
  manager <- newManager defaultManagerSettings
  --einitialRequest <- parseRequest $ T.unpack (url <> "/api/embed")
  eInitialRequest <-
    try $ parseRequest $ T.unpack (url <> "/api/embed") :: IO (Either HttpException Request)
  case eInitialRequest of
    Left e -> do
      return $ Left $ show e
    Right initialRequest -> do
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
      eResp <- try $ httpLbs request manager :: IO (Either HttpException (Response ByteString))
      case eResp of
        Left err -> return $ Left (show err)
        Right resp ->
          case decode (responseBody resp) of
            Nothing -> return $ Left $ "Couldn't decode response: " <> show (responseBody resp)
            Just r -> return $ Right r

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
