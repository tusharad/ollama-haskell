{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Ollama.Push
  ( -- * Push API
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
push ::
  -- | Model name
  Text ->
  -- | Insecure
  Maybe Bool ->
  -- | Stream
  Maybe Bool ->
  -- | Ollama config
  Maybe OllamaConfig ->
  IO ()
push modelName mInsecure mStream mbConfig = do
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

pushM :: MonadIO m => Text -> Maybe Bool -> Maybe Bool -> Maybe OllamaConfig -> m ()
pushM t insec s mbCfg = liftIO $ push t insec s mbCfg
