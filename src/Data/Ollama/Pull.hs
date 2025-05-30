{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Ollama.Pull
  ( -- * Downloaded Models
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

pullOps ::
  -- | Model Name
  Text ->
  -- | Insecure
  Maybe Bool ->
  -- | Stream
  Maybe Bool ->
  -- | Ollama Config
  Maybe OllamaConfig ->
  IO (Either OllamaError PullResp)
pullOps modelName mInsecure mStream mbConfig = do
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

pull ::
  -- | Model Name
  Text ->
  IO (Either OllamaError PullResp)
pull modelName = pullOps modelName Nothing Nothing Nothing

pullM :: MonadIO m => Text -> m (Either OllamaError PullResp)
pullM t = liftIO $ pull t

pullOpsM ::
  MonadIO m =>
  Text ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe OllamaConfig ->
  m (Either OllamaError PullResp)
pullOpsM t mbInsecure mbStream mbCfg = liftIO $ pullOps t mbInsecure mbStream mbCfg
