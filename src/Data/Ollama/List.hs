{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.List
  ( -- * List Models API
    list
  , listM
  , Models (..)
  , ModelInfo (..)
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Ollama.Common.Config (OllamaConfig)
import Data.Ollama.Common.Error (OllamaError)
import Data.Ollama.Common.Types as CT
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Time
import GHC.Int (Int64)

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

list ::
  -- | Ollama Config
  Maybe OllamaConfig ->
  IO (Either OllamaError Models)
list mbConfig = do
  withOllamaRequest
    "/api/tags"
    "GET"
    (Nothing :: Maybe Value)
    mbConfig
    commonNonStreamingHandler

listM :: MonadIO m => Maybe OllamaConfig -> m (Either OllamaError Models)
listM mbCfg = liftIO $ list mbCfg
