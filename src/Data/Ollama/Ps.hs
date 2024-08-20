{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Ps (
 ps,
 RunningModels(..),
 RunningModel(..),
) where

import Data.Aeson
import Data.Time
import Data.Ollama.Common.Utils as CU
import Data.Ollama.Common.Types as CT
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Text (Text)
import GHC.Int (Int64)

-- Types for Ps API
newtype RunningModels = RunningModels [RunningModel]
  deriving (Eq, Show)

data RunningModel = RunningModel
  { name_ :: Text,
    modelName :: Text,
    size_ :: Int64,
    modelDigest :: Text,
    modelDetails :: ModelDetails,
    expiresAt :: UTCTime,
    sizeVRam :: Int64
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

-- List Running Models
ps :: IO (Maybe RunningModels)
ps = do
  let url = CU.host defaultOllama
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ T.unpack (url <> "/api/ps")
  response <- httpLbs request manager
  if statusCode (responseStatus response) /= 200 then pure Nothing
    else do
      let res = decode (responseBody response) :: Maybe RunningModels
      case res of
        Nothing -> pure Nothing
        Just l -> pure $ Just l
