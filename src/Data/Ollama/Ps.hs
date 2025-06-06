{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Ollama.Ps
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Functionality for listing running models in the Ollama client.

This module provides functions to retrieve a list of models currently running on the Ollama server.
It includes both an IO-based function ('ps') and a monadic version ('psM') for use in 'MonadIO'
contexts. The operation is performed via a GET request to the @\/api\/ps@ endpoint, returning a
'RunningModels' type containing a list of 'RunningModel' records with details about each running model.

Example:

>>> ps Nothing
Right (RunningModels [RunningModel ...])
-}
module Data.Ollama.Ps
  ( -- * List Running Models API
    ps
  , psM

    -- * Model Types
  , RunningModels (..)
  , RunningModel (..)
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Ollama.Common.Config (OllamaConfig)
import Data.Ollama.Common.Error (OllamaError)
import Data.Ollama.Common.Types as CT
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Time
import GHC.Int (Int64)

-- | A wrapper type containing a list of running models.
newtype RunningModels
  = -- | List of 'RunningModel' records describing currently running models.
    RunningModels [RunningModel]
  deriving (Eq, Show)

-- | Details about a specific running model.
data RunningModel = RunningModel
  { name_ :: !Text
  -- ^ The name of the running model instance.
  , modelName :: !Text
  -- ^ The base model name (e.g., "gemma3").
  , size_ :: !Int64
  -- ^ The size of the model in bytes.
  , modelDigest :: !Text
  -- ^ The digest (hash) of the model.
  , modelDetails :: !ModelDetails
  -- ^ Additional details about the model (e.g., format, family, parameters).
  , expiresAt :: !UTCTime
  -- ^ The timestamp when the model's memory allocation expires.
  , sizeVRam :: !Int64
  -- ^ The size of the model's VRAM usage in bytes.
  }
  deriving (Eq, Show)

-- | JSON parsing instance for 'RunningModels'.
instance FromJSON RunningModels where
  parseJSON = withObject "Models" $ \v -> RunningModels <$> v .: "models"

-- | JSON parsing instance for 'RunningModel'.
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

{- | Retrieves a list of currently running models from the Ollama server.

Sends a GET request to the @\/api\/ps@ endpoint to fetch the list of running models.
Returns 'Right' with a 'RunningModels' containing the list of 'RunningModel' on success,
or 'Left' with an 'OllamaError' on failure.
Example:

>>> ps Nothing
Right (RunningModels [RunningModel {name_ = "gemma3:instance1", modelName = "gemma3", ...}])
-}
ps ::
  -- | Optional 'OllamaConfig' (defaults to 'defaultOllamaConfig' if 'Nothing')
  Maybe OllamaConfig ->
  IO (Either OllamaError RunningModels)
ps mbConfig = do
  withOllamaRequest
    "/api/ps"
    "GET"
    (Nothing :: Maybe Value)
    mbConfig
    commonNonStreamingHandler

{- | MonadIO version of 'ps' for use in monadic contexts.

Lifts the 'ps' function into a 'MonadIO' context, allowing it to be used in monadic computations.
-}
psM :: MonadIO m => Maybe OllamaConfig -> m (Either OllamaError RunningModels)
psM mbCfg = liftIO $ ps mbCfg
