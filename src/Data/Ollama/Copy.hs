{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Ollama.Copy
  ( -- * Copy Model API
    copyModel
  , copyModelM
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Ollama.Common.Config (OllamaConfig (..))
import Data.Ollama.Common.Error (OllamaError)
import Data.Ollama.Common.Utils (nonJsonHandler, withOllamaRequest)
import Data.Text (Text)
import GHC.Generics

data CopyModelOps = CopyModelOps
  { source :: !Text
  , destination :: !Text
  }
  deriving (Show, Eq, Generic, ToJSON)

-- | Copy model from source to destination
copyModel ::
  -- | Source model
  Text ->
  -- | Destination model
  Text ->
  -- | Ollama config
  Maybe OllamaConfig ->
  IO (Either OllamaError ())
copyModel
  source_
  destination_
  mbConfig = do
    let reqBody = CopyModelOps {source = source_, destination = destination_}
    withOllamaRequest
      "/api/copy"
      "POST"
      (Just reqBody)
      mbConfig
      (fmap ((const ()) <$>) . nonJsonHandler)

copyModelM :: MonadIO m => Text -> Text -> Maybe OllamaConfig -> m (Either OllamaError ())
copyModelM s d mbCfg = liftIO $ copyModel s d mbCfg
