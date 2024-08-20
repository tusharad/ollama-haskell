{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.List
  ( list,
    Models (..),
    ModelInfo (..),
  )
where

import Data.Aeson
import Data.Ollama.Common.Utils as CU
import Data.Ollama.Common.Types as CT
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Text (Text)
import Data.Time
import GHC.Int (Int64)

newtype Models = Models [ModelInfo]
  deriving (Eq, Show)

data ModelInfo = ModelInfo
  { name :: Text,
    modifiedAt :: UTCTime,
    size :: Int64,
    digest :: Text,
    details :: ModelDetails
  }
  deriving (Eq, Show)

-- Instances
instance FromJSON Models where
  parseJSON = withObject "Models" $ \v -> Models <$> v .: "models"

instance FromJSON ModelInfo where
  parseJSON = withObject "ModelInfo" $ \v ->
    ModelInfo
      <$> v .: "name"
      <*> v .: "modified_at"
      <*> v .: "size"
      <*> v .: "digest"
      <*> v .: "details"

list :: IO (Maybe Models)
list = do
  let url = CU.host defaultOllama
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ T.unpack (url <> "/api/tags")
  response <- httpLbs request manager
  if statusCode (responseStatus response) /= 200 then pure Nothing
    else do
      let res = decode (responseBody response) :: Maybe Models
      case res of
        Nothing -> pure Nothing
        Just l -> pure $ Just l
