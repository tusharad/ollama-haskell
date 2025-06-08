{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Ollama.Common.Config
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : A unified configuration type for controlling Ollama client behavior.

== Overview

This module defines the core configuration record used throughout the Ollama Haskell client.

Use 'defaultOllamaConfig' as a starting point and customize it with helper functions
like 'withOnModelStart', 'withOnModelFinish', or 'withOnModelError'.

Includes settings for base URL, timeout, retry logic, and custom HTTP managers.
-}
module Data.Ollama.Common.Config
  ( -- * Configuration Type
    OllamaConfig (..)

    -- * Default Config
  , defaultOllamaConfig

    -- * Hook Helpers
  , withOnModelStart
  , withOnModelFinish
  , withOnModelError
  ) where

import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client

{- | Configuration for the Ollama client.
Used across all requests to customize behavior such as timeouts, retries,
custom HTTP manager, and lifecycle hooks.

@since 0.2.0.0
-}
data OllamaConfig = OllamaConfig
  { hostUrl :: Text
  -- ^ Base URL for the Ollama server (default: @http://127.0.0.1:11434@)
  , timeout :: Int
  -- ^ Timeout in seconds for API requests (ignored if 'commonManager' is set)
  , onModelStart :: Maybe (IO ())
  -- ^ Callback executed when a model starts
  , onModelError :: Maybe (IO ())
  -- ^ Callback executed if a model encounters an error
  , onModelFinish :: Maybe (IO ())
  -- ^ Callback executed when a model finishes (not called on error)
  , retryCount :: Maybe Int
  -- ^ Number of retries on failure (default: @0@ if 'Nothing')
  , retryDelay :: Maybe Int
  -- ^ Delay between retries in seconds (if applicable)
  , commonManager :: Maybe Manager
  -- ^ Shared HTTP manager; disables timeout and retry settings
  }
  deriving (Generic)

{- | A default configuration pointing to @localhost:11434@ with 90s timeout
and no hooks or retry logic.
-}
defaultOllamaConfig :: OllamaConfig
defaultOllamaConfig =
  OllamaConfig
    { hostUrl = "http://127.0.0.1:11434"
    , timeout = 90
    , onModelStart = Nothing
    , onModelError = Nothing
    , onModelFinish = Nothing
    , retryCount = Nothing
    , retryDelay = Nothing
    , commonManager = Nothing
    }

-- | Add a callback to be executed when a model starts.
withOnModelStart :: IO () -> OllamaConfig -> OllamaConfig
withOnModelStart f cfg = cfg {onModelStart = Just f}

-- | Add a callback to be executed when a model errors.
withOnModelError :: IO () -> OllamaConfig -> OllamaConfig
withOnModelError f cfg = cfg {onModelError = Just f}

-- | Add a callback to be executed when a model finishes successfully.
withOnModelFinish :: IO () -> OllamaConfig -> OllamaConfig
withOnModelFinish f cfg = cfg {onModelFinish = Just f}
