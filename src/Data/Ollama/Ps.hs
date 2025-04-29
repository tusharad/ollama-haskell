{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Ps
  ( ps
  , RunningModels (..)
  , RunningModel (..)
  ) where

import Data.Aeson
import Data.Ollama.Common.Types as CT
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Maybe (fromMaybe)
import GHC.Int (Int64)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

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
ps :: IO (Maybe RunningModels)
ps = psOps Nothing



-- | List running models
psOps :: Maybe Text -> IO (Maybe RunningModels)
psOps hostUrl = do
  let url = fromMaybe defaultOllamaUrl hostUrl
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ T.unpack (url <> "/api/ps")
  response <- httpLbs request manager
  if statusCode (responseStatus response) /= 200
    then pure Nothing
    else do
      let res = decode (responseBody response) :: Maybe RunningModels
      case res of
        Nothing -> pure Nothing
        Just l -> pure $ Just l
