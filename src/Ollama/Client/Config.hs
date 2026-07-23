{- |
Module      : Ollama.Client.Config
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Client configuration settings, retry policies, and logging thresholds.

@since 1.0.0.0
-}
module Ollama.Client.Config (
  OllamaClientConfig (..),
  defaultConfig,
  RetryPolicy (..),
  LogLevel (..),
) where

import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Text (Text)
import Network.HTTP.Client (Manager)

{- | Logging levels for structured client events.

@since 1.0.0.0
-}
data LogLevel = Debug | Info | Warn | Error
  deriving stock (Eq, Ord, Show, Bounded, Enum)

{- | Configurable retry strategy for recoverable network errors.

@since 1.0.0.0
-}
data RetryPolicy
  = NoRetry
  | ConstantRetry !Int !Int
  | ExponentialRetry !Int !Int
  deriving stock (Eq, Show)

{- | Configuration settings for an 'OllamaClient'.

@since 1.0.0.0
-}
data OllamaClientConfig = OllamaClientConfig
  { configBaseUrl :: !Text
  , configTimeout :: !Int
  , configRetry :: !RetryPolicy
  , configManager :: !(Maybe Manager)
  , configHeaders :: ![(CI ByteString, ByteString)]
  , configApiKey :: !(Maybe Text)
  , configLogger :: !(Maybe (LogLevel -> Text -> IO ()))
  , configOnStart :: !(Maybe (IO ()))
  , configOnSuccess :: !(Maybe (IO ()))
  , configOnError :: !(Maybe (IO ()))
  }

{- | Default configuration connecting to @http://127.0.0.1:11434@ with 90s timeout.

@since 1.0.0.0
-}
defaultConfig :: OllamaClientConfig
defaultConfig =
  OllamaClientConfig
    { configBaseUrl = "http://127.0.0.1:11434"
    , configTimeout = 90
    , configRetry = NoRetry
    , configManager = Nothing
    , configHeaders = []
    , configApiKey = Nothing
    , configLogger = Nothing
    , configOnStart = Nothing
    , configOnSuccess = Nothing
    , configOnError = Nothing
    }
