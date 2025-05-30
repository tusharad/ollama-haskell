{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Ollama.Delete
  ( -- * Delete downloaded Models
    deleteModel
  , deleteModelM
  ) where

import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Ollama.Common.Config (OllamaConfig (..), defaultOllamaConfig)
import Data.Ollama.Common.Error qualified as Error
import Data.Ollama.Common.Utils (withRetry)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (status404)

-- TODO: Add Options parameter
-- TODO: Add Context parameter
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
  IO ()
deleteModel modelName mbOllamaConfig = do
  let OllamaConfig {..} = fromMaybe defaultOllamaConfig mbOllamaConfig
      timeoutMicros = timeout * 60 * 1000000
  manager <-
    newTlsManagerWith
      defaultManagerSettings
        { managerResponseTimeout = responseTimeoutMicro timeoutMicros
        }
  initialRequest <- parseRequest $ T.unpack (hostUrl <> "/api/delete")
  let reqBody = DeleteModelReq {name = modelName}
      request = initialRequest {method = "POST", requestBody = RequestBodyLBS (encode reqBody)}
      retryCnt = fromMaybe 0 retryCount
      retryDelay_ = fromMaybe 1 retryDelay
  void $ withRetry retryCnt retryDelay_ $ do
    maybe (pure ()) id onModelStart
    eResponse <- try $ httpLbs request manager
    case eResponse of
      Left ex -> do
        fromMaybe (pure ()) onModelError
        return $ Left $ Error.HttpError ex
      Right response -> do
        fromMaybe (pure ()) onModelFinish
        if responseStatus response == status404
          then pure $ Right ()
          else do
            fromMaybe (pure ()) onModelError
            pure $ Left $ Error.ApiError "Source Model does not exist"

deleteModelM :: MonadIO m => Text -> Maybe OllamaConfig -> m ()
deleteModelM t mbCfg = liftIO $ deleteModel t mbCfg
