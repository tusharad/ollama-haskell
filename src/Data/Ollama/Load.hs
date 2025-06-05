{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Ollama.Load
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : High-level functions for loading and unloading models in the Ollama client.

This module provides functions to load and unload generative models in the Ollama server.
It includes both IO-based functions ('loadGenModel', 'unloadGenModel') and monadic versions
('loadGenModelM', 'unloadGenModelM') for use in 'MonadIO' contexts. The operations are
performed via POST requests to the "/api//generate" endpoint, leveraging the 'GenerateOps'
configuration from the 'Data.Ollama.Generate' module.

Loading a model keeps it in memory for faster subsequent requests, while unloading frees
up memory by setting the keep-alive duration to zero.

Example:

>>> loadGenModel "gemma3"
Right ()
>>> unloadGenModel "gemma3"
Right ()
-}
module Data.Ollama.Load
  ( -- * Load and Unload Model APIs
    loadGenModel
  , unloadGenModel
  , loadGenModelM
  , unloadGenModelM
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Ollama.Common.Error
import Data.Ollama.Common.Utils (commonNonStreamingHandler, withOllamaRequest)
import Data.Ollama.Generate qualified as Gen
import Data.Text (Text)

{- | Loads a generative model into memory.

Sends a POST request to the "/api//generate" endpoint to load the specified model into
memory, ensuring faster response times for subsequent requests. Returns 'Right ()' on
success or 'Left' with an 'OllamaError' on failure.
--
-- @since 0.2.0.0
-}
loadGenModel ::
  -- |  Model name (e.g., "gemma3")
  Text ->
  IO (Either OllamaError ())
loadGenModel m = do
  let ops = Gen.defaultGenerateOps {Gen.modelName = m}
  withOllamaRequest "/api//generate" "POST" (Just ops) Nothing commonNonStreamingHandler

{- | Unloads a generative model from memory.

Sends a POST request to the "/api//generate" endpoint with a keep-alive duration of zero
to unload the specified model from memory, freeing up resources. Returns 'Right ()' on
success or 'Left' with an 'OllamaError' on failure.
--
-- @since 0.2.0.0
-}
unloadGenModel ::
  -- | Model name (e.g., "gemma3")
  Text ->
  IO (Either OllamaError ())
unloadGenModel m = do
  let ops = Gen.defaultGenerateOps {Gen.modelName = m, Gen.keepAlive = Just 0}
  withOllamaRequest "/api//generate" "POST" (Just ops) Nothing commonNonStreamingHandler

{- | MonadIO version of 'loadGenModel' for use in monadic contexts.

Lifts the 'loadGenModel' function into a 'MonadIO' context, allowing it to be used in
monadic computations.

Example:

>>> import Control.Monad.IO.Class
>>> runReaderT (loadGenModelM "gemma3") someContext
Right ()
--
-- @since 0.2.0.0
-}
loadGenModelM :: MonadIO m => Text -> m (Either OllamaError ())
loadGenModelM t = liftIO $ loadGenModel t

{- | MonadIO version of 'unloadGenModel' for use in monadic contexts.

Lifts the 'unloadGenModel' function into a 'MonadIO' context, allowing it to be used in
monadic computations.

Example:

>>> import Control.Monad.IO.Class
>>> runReaderT (unloadGenModelM "gemma3") someContext
Right ()
--
-- @since 0.2.0.0
-}
unloadGenModelM :: MonadIO m => Text -> m (Either OllamaError ())
unloadGenModelM t = liftIO $ unloadGenModel t
