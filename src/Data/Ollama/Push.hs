{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Push
  ( -- * Push API
    push
  , pushOps
  ) where

import Data.Aeson
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import GHC.Generics
import GHC.Int (Int64)
import Control.Monad (void)
import Data.Ollama.Common.Types (HasDone (getDone))
import Data.Ollama.Common.Config (OllamaConfig)

-- TODO: Add Options parameter
data PushOps = PushOps
  { name :: !Text
  , insecure :: !(Maybe Bool)
  , stream :: !(Maybe Bool)
  }
  deriving (Show, Eq, Generic, ToJSON)

data PushResp = PushResp
  { status :: !Text
  , digest :: !(Maybe Text)
  , total :: !(Maybe Int64)
  }
  deriving (Show, Eq, Generic, FromJSON)

instance HasDone PushResp where
  getDone PushResp {..} = status /= "success"

-- | Push a model with options
pushOps ::
  -- | Model name
  Text ->
  -- | Insecure
  Maybe Bool ->
  -- | Stream
  Maybe Bool ->
  -- | Ollama config
  Maybe OllamaConfig ->
  IO ()
pushOps modelName mInsecure mStream mbConfig = do
  void $
    withOllamaRequest
      "/api/pull"
      "POST"
      (Just $ PushOps {name = modelName, insecure = mInsecure, stream = mStream})
      mbConfig
      (commonStreamHandler onToken onComplete)
  where
    onToken :: PushResp -> IO ()
    onToken _ = putStrLn $ "Pushing... "

    onComplete :: IO ()
    onComplete = putStrLn "Completed"
-- Higher level API for Pull
-- This API is untested. Will test soon!

-- | Push a model
push ::
  -- | Model name
  Text ->
  IO ()
push modelName = pushOps modelName Nothing Nothing Nothing 
