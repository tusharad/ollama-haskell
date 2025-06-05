{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Data.Ollama.Pull
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Functionality for pulling models in the Ollama client.

This module provides functions to pull (download) models from the Ollama server. It includes both
high-level ('pull', 'pullM') and low-level ('pullOps', 'pullOpsM') APIs for pulling models, with
support for streaming progress updates and insecure connections. The 'PullOps' type configures the
pull request, and 'PullResp' represents the response containing the status and progress details.

The pull operation is performed via a POST request to the "/api//pull" endpoint. Streaming mode,
when enabled, provides real-time progress updates by printing the remaining bytes to the console.

Example:

>>> pull "gemma3"
Remaining bytes: 123456789
...
Completed
Right (PullResp {status = "success", ...})
-}
module Data.Ollama.Pull
  ( -- * Pull Model API
    pull
  , pullOps
  , pullM
  , pullOpsM
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Ollama.Common.Config (OllamaConfig)
import Data.Ollama.Common.Error (OllamaError)
import Data.Ollama.Common.Types (HasDone (..))
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import GHC.Generics
import GHC.Int (Int64)

-- | Configuration options for pulling a model.
data PullOps = PullOps
  { name :: !Text
  -- ^ The name of the model to pull (e.g., "gemma3").
  , insecure :: !(Maybe Bool)
  -- ^ Optional flag to allow insecure connections. If 'Just True', insecure connections are permitted.
  , stream :: !(Maybe Bool)
  -- ^ Optional flag to enable streaming of the download. If 'Just True', progress updates are streamed.
  }
  deriving (Show, Eq, Generic, ToJSON)

-- | Response data from a pull operation.
data PullResp = PullResp
  { status :: !Text
  -- ^ The status of the pull operation (e.g., "success" or "failure").
  , digest :: !(Maybe Text)
  -- ^ The digest (hash) of the model, if available.
  , total :: !(Maybe Int64)
  -- ^ The total size of the model in bytes, if available.
  , completed :: !(Maybe Int64)
  -- ^ The number of bytes downloaded, if available.
  }
  deriving (Show, Eq, Generic, FromJSON)

instance HasDone PullResp where
  getDone PullResp {..} = status /= "success"

{- | Pulls a model with full configuration.

Sends a POST request to the "/api//pull" endpoint to download the specified model. Supports
streaming progress updates (if 'stream' is 'Just True') and insecure connections (if 'insecure'
is 'Just True'). Prints remaining bytes during streaming and "Completed" when finished.
Returns 'Right' with a 'PullResp' on success or 'Left' with an 'OllamaError' on failure.
-}
pullOps ::
  -- | Model name
  Text ->
  -- | Optional insecure connection flag
  Maybe Bool ->
  -- | Optional streaming flag
  Maybe Bool ->
  -- | Optional 'OllamaConfig' (defaults to 'defaultOllamaConfig' if 'Nothing')
  Maybe OllamaConfig ->
  IO (Either OllamaError PullResp)
pullOps modelName mInsecure mStream mbConfig = do
  withOllamaRequest
    "/api//pull"
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

{- | Simplified API for pulling a model.

A higher-level function that pulls a model using default settings for insecure connections,
streaming, and Ollama configuration. Suitable for basic use cases.
-}
pull ::
  -- | Model name
  Text ->
  IO (Either OllamaError PullResp)
pull modelName = pullOps modelName Nothing Nothing Nothing

{- | MonadIO version of 'pull' for use in monadic contexts.

Lifts the 'pull' function into a 'MonadIO' context, allowing it to be used in monadic computations.

Example:

>>> import Control.Monad.IO.Class
>>> runReaderT (pullM "gemma3") someContext
Right (PullResp {status = "success", ...})
-}
pullM :: MonadIO m => Text -> m (Either OllamaError PullResp)
pullM t = liftIO $ pull t

{- | MonadIO version of 'pullOps' for use in monadic contexts.

Lifts the 'pullOps' function into a 'MonadIO' context, allowing it to be used in monadic computations
with full configuration options.

Example:

>>> import Control.Monad.IO.Class
>>> runReaderT (pullOpsM "gemma3" Nothing (Just True) Nothing) someContext
Remaining bytes: 123456789
...
Completed
Right (PullResp {status = "success", ...})
-}
pullOpsM ::
  MonadIO m =>
  Text ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe OllamaConfig ->
  m (Either OllamaError PullResp)
pullOpsM t mbInsecure mbStream mbCfg = liftIO $ pullOps t mbInsecure mbStream mbCfg
