{- |
Module      : Ollama.Types.Model
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Model info, details, listing, and metadata types.

@since 1.0.0.0
-}
module Ollama.Types.Model (
  ModelDetails (..),
  ModelInfo (..),
  ListResponse (..),
  RunningModel (..),
  RunningModelsResponse (..),
) where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Ollama.Types.Common (Digest, ModelName)

{- | Detailed specifications of a model's architecture and family.

@since 1.0.0.0
-}
data ModelDetails = ModelDetails
  { parentModel :: !(Maybe Text)
  , format :: !Text
  , family :: !Text
  , families :: ![Text]
  , parameterSize :: !Text
  , quantizationLevel :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ModelDetails where
  parseJSON = withObject "ModelDetails" $ \v ->
    ModelDetails
      <$> v .:? "parent_model"
      <*> v .:? "format" .!= ""
      <*> v .:? "family" .!= ""
      <*> v .:? "families" .!= []
      <*> v .:? "parameter_size" .!= ""
      <*> v .:? "quantization_level" .!= ""

instance ToJSON ModelDetails where
  toJSON ModelDetails {..} =
    object
      [ "parent_model" .= parentModel
      , "format" .= format
      , "family" .= family
      , "families" .= families
      , "parameter_size" .= parameterSize
      , "quantization_level" .= quantizationLevel
      ]

{- | Summary information for an installed local model.

@since 1.0.0.0
-}
data ModelInfo = ModelInfo
  { miName :: !ModelName
  , miModel :: !ModelName
  , miModifiedAt :: !UTCTime
  , miSize :: !Int64
  , miDigest :: !Digest
  , miDetails :: !ModelDetails
  , miCapabilities :: !(Maybe [Text])
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ModelInfo where
  parseJSON = withObject "ModelInfo" $ \v ->
    ModelInfo
      <$> v .: "name"
      <*> v .: "model"
      <*> v .: "modified_at"
      <*> v .: "size"
      <*> v .: "digest"
      <*> v .: "details"
      <*> v .:? "capabilities"

instance ToJSON ModelInfo where
  toJSON ModelInfo {..} =
    object
      [ "name" .= miName
      , "model" .= miModel
      , "modified_at" .= miModifiedAt
      , "size" .= miSize
      , "digest" .= miDigest
      , "details" .= miDetails
      , "capabilities" .= miCapabilities
      ]

{- | Response listing available local models.

@since 1.0.0.0
-}
newtype ListResponse = ListResponse
  { models :: [ModelInfo]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Summary information for a model currently loaded in memory.

@since 1.0.0.0
-}
data RunningModel = RunningModel
  { rmName :: !ModelName
  , rmModel :: !ModelName
  , rmSize :: !Int64
  , rmDigest :: !Digest
  , rmDetails :: !ModelDetails
  , rmExpiresAt :: !UTCTime
  , rmSizeVram :: !Int64
  }
  deriving stock (Eq, Show, Generic)

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

instance ToJSON RunningModel where
  toJSON RunningModel {..} =
    object
      [ "name" .= rmName
      , "model" .= rmModel
      , "size" .= rmSize
      , "digest" .= rmDigest
      , "details" .= rmDetails
      , "expires_at" .= rmExpiresAt
      , "size_vram" .= rmSizeVram
      ]

{- | Response listing loaded running models.

@since 1.0.0.0
-}
newtype RunningModelsResponse = RunningModelsResponse
  { runningModels :: [RunningModel]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON RunningModelsResponse where
  parseJSON = withObject "RunningModelsResponse" $ \v ->
    RunningModelsResponse <$> v .: "models"

instance ToJSON RunningModelsResponse where
  toJSON RunningModelsResponse {..} =
    object ["models" .= runningModels]
