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
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (status404)

-- TODO: Add Options parameter
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
  IO ()
copyModel
  source_
  destination_
  mbOllamaConfig = do
    let OllamaConfig {..} = fromMaybe defaultOllamaConfig mbOllamaConfig
        timeoutMicros = timeout * 60 * 1000000
    manager <-
      newTlsManagerWith
        defaultManagerSettings
          { managerResponseTimeout = responseTimeoutMicro timeoutMicros
          }
    initialRequest <- parseRequest $ T.unpack (hostUrl <> "/api/copy")
    let reqBody = CopyModelOps {source = source_, destination = destination_}
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

copyModelM :: MonadIO m => Text -> Text -> Maybe OllamaConfig -> m ()
copyModelM s d mbCfg = liftIO $ copyModel s d mbCfg
