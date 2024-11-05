{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Common.Types
  ( ModelDetails (..)
  , OllamaClient (..)
  ) where

import Data.Aeson
import Data.Text (Text)

data ModelDetails = ModelDetails
  { parentModel :: Maybe Text
  , format :: Text
  , familiy :: Text
  , families :: [Text]
  , parameterSize :: Text
  , quantizationLevel :: Text
  }
  deriving (Eq, Show)

instance FromJSON ModelDetails where
  parseJSON = withObject "ModelDetails" $ \v ->
    ModelDetails
      <$> v .: "parent_model"
      <*> v .: "format"
      <*> v .: "family"
      <*> v .:? "families" .!= []
      <*> v .: "parameter_size"
      <*> v .: "quantization_level"

newtype OllamaClient = OllamaClient
  { host :: Text
  }
  deriving (Eq, Show)
