{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Data.Ollama.Load
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Description: A high level module for load and unload model functions
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental
-}
module Data.Ollama.Load
  ( loadGenModel
  , unloadGenModel
  , loadGenModelM
  , unloadGenModelM
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Ollama.Common.Error
import Data.Ollama.Common.Utils (commonNonStreamingHandler, withOllamaRequest)
import Data.Ollama.Generate qualified as Gen
import Data.Text (Text)

-- | load generating model in memory
loadGenModel :: Text -> IO (Either OllamaError ())
loadGenModel m = do
  let ops = Gen.defaultGenerateOps {Gen.modelName = m}
  withOllamaRequest "/api/generate" "POST" (Just ops) Nothing commonNonStreamingHandler

-- | unload generating model from memory
unloadGenModel :: Text -> IO (Either OllamaError ())
unloadGenModel m = do
  let ops = Gen.defaultGenerateOps {Gen.modelName = m, Gen.keepAlive = Just 0}
  withOllamaRequest "/api/generate" "POST" (Just ops) Nothing commonNonStreamingHandler

loadGenModelM :: MonadIO m => Text -> m (Either OllamaError ())
loadGenModelM t = liftIO $ loadGenModel t

unloadGenModelM :: MonadIO m => Text -> m (Either OllamaError ())
unloadGenModelM t = liftIO $ unloadGenModel t
