{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Delete
  ( -- * Delete downloaded Models
    deleteModel
  , deleteModelOps
  ) where

import Control.Monad (when)
import Data.Aeson
import Data.Ollama.Common.Utils qualified as CU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (fromMaybe)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (status404)
import Data.Ollama.Common.Config (OllamaConfig(..))

-- TODO: Add Options parameter
-- TODO: Add Context parameter
newtype DeleteModelReq = DeleteModelReq {name :: Text}
  deriving newtype (Show, Eq)

instance ToJSON DeleteModelReq where
  toJSON
    ( DeleteModelReq
        name_
      ) =
      object
        [ "name" .= name_
        ]


deleteModel :: Text -> IO ()
deleteModel t = deleteModelOps t Nothing


-- | Delete a model
deleteModelOps ::
  -- | Model name
  Text ->
  -- | Ollama config
  Maybe OllamaConfig ->
  IO ()
deleteModelOps modelName mbOllamaConfig = do
    let url = fromMaybe CU.defaultOllamaUrl (hostUrl <$> mbOllamaConfig)
        timeoutMicros = let t = maybe 15 timeout mbOllamaConfig in t * 60 * 1000000
    manager <- newTlsManagerWith defaultManagerSettings
                { managerResponseTimeout = responseTimeoutMicro timeoutMicros}
    initialRequest <- parseRequest $ T.unpack (url <> "/api/delete")
    let reqBody =
          DeleteModelReq {name = modelName}
        request =
          initialRequest
            { method = "DELETE"
            , requestBody = RequestBodyLBS $ encode reqBody
            }
    maybe (pure ()) (mapM_ id . onModelStart) mbOllamaConfig
    response <- httpLbs request manager
    maybe (pure ()) (mapM_ id . onModelFinish) mbOllamaConfig
    when
      (responseStatus response == status404)
      (do 
        maybe (pure ()) (mapM_ id . onModelError) mbOllamaConfig 
        putStrLn "Model does not exist")
