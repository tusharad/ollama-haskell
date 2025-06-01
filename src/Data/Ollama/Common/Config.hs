{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module:      Data.Ollama.Common.Config
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Description: A unified configuration managment type
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental
-}
module Data.Ollama.Common.Config (
    OllamaConfig (..)
  , defaultOllamaConfig
  , setNContext
  , insert
  , withOnModelStart
  , withOnModelFinish
  , withOnModelError
 ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (fromText)
import Network.HTTP.Client

-- | Configuration for Ollama client
data OllamaConfig = OllamaConfig
  { hostUrl       :: Text -- hostUrl
  , timeout       :: Int  -- ^ Timeout in minutes
  , onModelStart  :: Maybe (IO ())  -- ^ Called when model starts (model name)
  , onModelError  :: Maybe (IO ())  -- ^ Called on error (error message)
  , onModelFinish :: Maybe (IO ())  -- ^ Called on completion (start, end)
  , retryCount    :: Maybe Int      -- ^ Retry if any error happened. Default 0
  , retryDelay    :: Maybe Int      -- ^ How many seconds later retry should happen
  , commonManager :: Maybe Manager
  } deriving (Generic)

-- | Default configuration
defaultOllamaConfig :: OllamaConfig
defaultOllamaConfig = OllamaConfig
  { hostUrl = "http://127.0.0.1:11434"
  , timeout = 15
  , onModelStart = Nothing
  , onModelError = Nothing
  , onModelFinish = Nothing
  , retryCount = Nothing
  , retryDelay = Nothing
  , commonManager = Nothing
  }

-- | Set context length in model options
setNContext :: Int -> Value -> Value
setNContext n val = insert "n_ctx" n val

-- | Helper for inserting into object (simplified)
insert :: ToJSON a => Text -> a -> Value -> Value
insert k v (Object obj) = Object $ KM.insert (fromText k) (toJSON v) obj
insert _ _ _ = error "Expected object"

-- | Add model start callback
withOnModelStart :: IO () -> OllamaConfig -> OllamaConfig
withOnModelStart f cfg = cfg { onModelStart = Just f }

-- | Add model error callback
withOnModelError :: IO () -> OllamaConfig -> OllamaConfig
withOnModelError f cfg = cfg { onModelError = Just f }

-- | Add model finish callback
withOnModelFinish :: IO () -> OllamaConfig -> OllamaConfig
withOnModelFinish f cfg = cfg { onModelFinish = Just f }
