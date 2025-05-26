{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Ps
  ( ps
  , psOps
  , RunningModels (..)
  , RunningModel (..)
  ) where

import Data.Aeson
import Data.Ollama.Common.Types as CT
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Time
import GHC.Int (Int64)
import Data.Ollama.Common.Error (OllamaError)

-- Types for Ps API
newtype RunningModels = RunningModels [RunningModel]
  deriving (Eq, Show)

data RunningModel = RunningModel
  { name_ :: !Text
  , modelName :: !Text
  , size_ :: !Int64
  , modelDigest :: !Text
  , modelDetails :: !ModelDetails
  , expiresAt :: !UTCTime
  , sizeVRam :: !Int64
  }
  deriving (Eq, Show)

instance FromJSON RunningModels where
  parseJSON = withObject "Models" $ \v -> RunningModels <$> v .: "models"

instance FromJSON RunningModel where
  parseJSON = withObject "RunningModel" $ \v ->
    RunningModel
      <$> v .: "name"
      <*> v .: "model"
      <*> v .: "size"
      <*> v .: "digest"
      <*> v .: "details"
      <*> v .: "expires_at"
      <*> v .: "size_vram"

-- | List running models
ps :: IO (Either OllamaError RunningModels)
ps = psOps Nothing

-- | List running models
psOps :: Maybe Text -> IO (Either OllamaError RunningModels)
psOps hostUrl = do
  withOllamaRequest
    "/api/ps"
    "GET"
    (Nothing :: Maybe Value)
    hostUrl 
    Nothing
    commonNonStreamingHandler
