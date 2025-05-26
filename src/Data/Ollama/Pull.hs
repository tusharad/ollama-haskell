{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Ollama.Pull
  ( -- * Downloaded Models
    pull
  , pullOps
  ) where

import Control.Monad (void)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Ollama.Common.Types (HasDone (..))
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import GHC.Generics
import GHC.Int (Int64)
import Data.Ollama.Common.Config (OllamaConfig)

-- TODO: Add Options parameter

-- | Configuration options for pulling a model.
data PullOps = PullOps
  { name :: !Text
  -- ^ The name of the model to pull.
  , insecure :: !(Maybe Bool)
  -- ^ Option to allow insecure connections.
  -- If set to 'Just True', the pull operation will allow insecure connections.
  , stream :: !(Maybe Bool)
  -- ^ Option to enable streaming of the download.
  -- If set to 'Just True', the download will be streamed.
  }
  deriving (Show, Eq, Generic, ToJSON)

-- | Response data from a pull operation.
data PullResp = PullResp
  { status :: !Text
  -- ^ The status of the pull operation, e.g., "success" or "failure".
  , digest :: !(Maybe Text)
  -- ^ The digest of the model, if available.
  , total :: !(Maybe Int64)
  -- ^ The total size of the model in bytes, if available.
  , completed :: !(Maybe Int64)
  -- ^ The number of bytes completed, if available.
  }
  deriving (Show, Eq, Generic, FromJSON)

instance HasDone PullResp where
  getDone PullResp {..} = status /= "success"

{- |
Pull a model with additional options for insecure connections and streaming.
This function interacts directly with the Ollama API to download the specified model.

Example:

> pullOps "myModel" (Just True) (Just True)

This will attempt to pull "myModel" with insecure connections allowed and enable streaming.
-}
pullOps ::
  -- | Model Name
  Text ->
  -- | Insecure
  Maybe Bool ->
  -- | Stream
  Maybe Bool ->
  -- | Ollama Config
  Maybe OllamaConfig ->
  IO ()
pullOps modelName mInsecure mStream mbConfig = do
  void $
    withOllamaRequest
      "/api/pull"
      "POST"
      (Just $ PullOps {name = modelName, insecure = mInsecure, stream = mStream})
      mbConfig
      (commonStreamHandler onToken onComplete)
  where
    onToken :: PullResp -> IO ()
    onToken res = do
      let completed' = fromMaybe 0 (completed res)
      let total' = fromMaybe 0 (total res)
      putStrLn $ "Remaining bytes: " <> show (total' - completed')

    onComplete :: IO ()
    onComplete = putStrLn "Completed"

{- |
Pull a model using default options. This simplifies the pull operation by
not requiring additional options.

Example:

> pull "myModel"

This will pull "myModel" using default settings (no insecure connections and no streaming).
-}
pull ::
  -- | Model Name
  Text ->
  IO ()
pull modelName = pullOps modelName Nothing Nothing Nothing
