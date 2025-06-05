{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Data.Ollama.Push
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Functionality for pushing models to the Ollama server.

This module provides functions to push (upload) a model to the Ollama server. It includes
both an IO-based function ('push') and a monadic version ('pushM') for use in 'MonadIO'
contexts. The push operation is performed via a POST request to the "/api//pull" endpoint,
with support for streaming progress updates and insecure connections.

The 'PushOps' type configures the push request, and 'PushResp' represents the response
containing the status and progress details. Streaming mode, when enabled, provides
real-time progress updates by printing to the console.

Example:

>>> push "gemma3" Nothing (Just True) Nothing
Pushing...
Completed
-}
module Data.Ollama.Push
  ( -- * Push Model API
    push
  , pushM
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Ollama.Common.Config (OllamaConfig)
import Data.Ollama.Common.Types (HasDone (getDone))
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import GHC.Generics
import GHC.Int (Int64)

-- | Configuration options for pushing a model.
data PushOps = PushOps
  { name :: !Text
  -- ^ The name of the model to push (e.g., "gemma3").
  , insecure :: !(Maybe Bool)
  -- ^ Optional flag to allow insecure connections.
  -- If 'Just True', insecure connections are permitted.
  , stream :: !(Maybe Bool)
  -- ^ Optional flag to enable streaming of the upload.
  -- If 'Just True', progress updates are streamed.
  }
  deriving (Show, Eq, Generic, ToJSON)

-- | Response data from a push operation.
data PushResp = PushResp
  { status :: !Text
  -- ^ The status of the push operation (e.g., "success" or "failure").
  , digest :: !(Maybe Text)
  -- ^ The digest (hash) of the model, if available.
  , total :: !(Maybe Int64)
  -- ^ The total size of the model in bytes, if available.
  }
  deriving (Show, Eq, Generic, FromJSON)

instance HasDone PushResp where
  getDone PushResp {..} = status /= "success"

{- | Pushes a model to the Ollama server with specified options.

Sends a POST request to the "/api//pull" endpoint to upload the specified model. Supports
streaming progress updates (if 'stream' is 'Just True') and insecure connections (if
'insecure' is 'Just True'). Prints "Pushing..." during streaming and "Completed" when
finished. Returns '()' on completion.
-}
push ::
  -- | Model name
  Text ->
  -- | Optional insecure connection flag
  Maybe Bool ->
  -- | Optional streaming flag
  Maybe Bool ->
  -- | Optional 'OllamaConfig' (defaults to 'defaultOllamaConfig' if 'Nothing')
  Maybe OllamaConfig ->
  IO ()
push modelName mInsecure mStream mbConfig = do
  void $
    withOllamaRequest
      "/api//push"
      "POST"
      (Just $ PushOps {name = modelName, insecure = mInsecure, stream = mStream})
      mbConfig
      (commonStreamHandler onToken onComplete)
  where
    onToken :: PushResp -> IO ()
    onToken _ = putStrLn "Pushing... "

    onComplete :: IO ()
    onComplete = putStrLn "Completed"

{- | MonadIO version of 'push' for use in monadic contexts.

Lifts the 'push' function into a 'MonadIO' context, allowing it to be used in monadic
computations.

Example:

>>> import Control.Monad.IO.Class
>>> runReaderT (pushM "gemma3" Nothing (Just True) Nothing) someContext
Pushing...
Completed
-}
pushM :: MonadIO m => Text -> Maybe Bool -> Maybe Bool -> Maybe OllamaConfig -> m ()
pushM t insec s mbCfg = liftIO $ push t insec s mbCfg
