{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.List
  ( -- * List Models API
    list
  , listOps
  , Models (..)
  , ModelInfo (..)
  )
where

import Data.Aeson
import Data.Ollama.Common.Types as CT
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Time
import GHC.Int (Int64)
import Data.Ollama.Common.Error (OllamaError)

newtype Models = Models [ModelInfo]
  deriving (Eq, Show)

data ModelInfo = ModelInfo
  { name :: !Text
  , modifiedAt :: !UTCTime
  , size :: !Int64
  , digest :: !Text
  , details :: !ModelDetails
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

-- | List all models from local
list :: IO (Either OllamaError Models)
list = listOps Nothing

listOps ::
  -- | Ollama URL
  Maybe Text ->
  IO (Either OllamaError Models)
listOps hostUrl = do
  withOllamaRequest
    "/api/tags"
    "GET"
    (Nothing :: Maybe Value)
    hostUrl
    Nothing
    commonNonStreamingHandler
