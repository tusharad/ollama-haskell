{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Delete
  ( -- * Delete downloaded Models
    deleteModel
  , deleteModelM
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Ollama.Common.Config (OllamaConfig (..))
import Data.Ollama.Common.Error (OllamaError)
import Data.Ollama.Common.Utils (nonJsonHandler, withOllamaRequest)
import Data.Text (Text)

newtype DeleteModelReq = DeleteModelReq {name :: Text}
  deriving newtype (Show, Eq)

instance ToJSON DeleteModelReq where
  toJSON (DeleteModelReq name_) = object ["name" .= name_]

-- | Delete a model
deleteModel ::
  -- | Model name
  Text ->
  -- | Ollama config
  Maybe OllamaConfig ->
  IO (Either OllamaError ())
deleteModel modelName mbConfig = do
  let reqBody = DeleteModelReq {name = modelName}
  withOllamaRequest
    "/api/delete"
    "DELETE"
    (Just reqBody)
    mbConfig
    (fmap ((const ()) <$>) . nonJsonHandler)

deleteModelM :: MonadIO m => Text -> Maybe OllamaConfig -> m (Either OllamaError ())
deleteModelM t mbCfg = liftIO $ deleteModel t mbCfg
