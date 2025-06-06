-- |
-- Module      : Data.Ollama.List
-- Copyright   : (c) 2025 Tushar Adhatrao
-- License     : MIT
-- Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
-- Stability   : experimental
-- Description : Functionality for listing available models in the Ollama client.
--
-- This module provides functions to retrieve a list of models available on the Ollama server.
-- It includes both an IO-based function ('list') and a monadic version ('listM') for use in
-- 'MonadIO' contexts. The list operation is performed via a GET request to the @\/api\/tags@ endpoint,
-- returning a 'Models' type containing a list of 'ModelInfo' records with details about each model.
--
-- Example:
--
-- >>> list Nothing
-- Right (Models [ModelInfo ...])
--
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.List
  ( -- * List Models API
    list,
    listM,

    -- * Model Types
    Models (..),
    ModelInfo (..)
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

-- | A wrapper type containing a list of available models.
--
newtype Models = Models [ModelInfo]
  -- ^ List of 'ModelInfo' records describing available models.
  deriving (Eq, Show)

-- | Details about a specific model.
--
data ModelInfo = ModelInfo
  { name :: !Text
    -- ^ The name of the model.
  , modifiedAt :: !UTCTime
    -- ^ The timestamp when the model was last modified.
  , size :: !Int64
    -- ^ The size of the model in bytes.
  , digest :: !Text
    -- ^ The digest (hash) of the model.
  , details :: !ModelDetails
    -- ^ Additional details about the model (e.g., format, family, parameters).
  } deriving (Eq, Show)

-- | JSON parsing instance for 'Models'.
instance FromJSON Models where
  parseJSON = withObject "Models" $ \v -> Models <$> v .: "models"

-- | JSON parsing instance for 'ModelInfo'.
instance FromJSON ModelInfo where
  parseJSON = withObject "ModelInfo" $ \v ->
    ModelInfo
      <$> v .: "name"
      <*> v .: "modified_at"
      <*> v .: "size"
      <*> v .: "digest"
      <*> v .: "details"

-- | Retrieves a list of available models from the Ollama server.
--
-- Sends a GET request to the @\/api\/tags@ endpoint to fetch the list of models.
-- Returns 'Right' with a 'Models' containing the list of 'ModelInfo' on success,
-- or 'Left' with an 'OllamaError' on failure.
list ::
  Maybe OllamaConfig -> -- ^ Optional 'OllamaConfig' (defaults to 'defaultOllamaConfig' if 'Nothing')
  IO (Either OllamaError Models)
list mbConfig = do
  withOllamaRequest
    "/api/tags"
    "GET"
    (Nothing :: Maybe Value)
    mbConfig
    commonNonStreamingHandler

-- | MonadIO version of 'list' for use in monadic contexts.
--
-- Lifts the 'list' function into a 'MonadIO' context, allowing it to be used in monadic computations.
--
-- Example:
--
-- >>> import Control.Monad.IO.Class
-- >>> runReaderT (listM Nothing) someContext
-- Right (Models [ModelInfo ...])
--
listM :: MonadIO m => Maybe OllamaConfig -> m (Either OllamaError Models)
listM mbCfg = liftIO $ list mbCfg