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
import Network.HTTP.Client
import Network.HTTP.Types.Status (status404)

-- TODO: Add Options parameter
-- TODO: Add Context parameter
newtype DeleteModelReq = DeleteModelReq {name :: Text}
  deriving newtype (Show, Eq, ToJSON)


deleteModel :: Text -> IO ()
deleteModel = deleteModelOps Nothing


-- | Delete a model
deleteModelOps ::
  -- | Ollama URL
  Maybe Text ->
  -- | Model name
  Text ->
  IO ()
deleteModel hostUrl modelName =
  do
    let url = fromMaybe hostUrl CU.defaultOllamaUrl
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest $ T.unpack (url <> "/api/delete")
    let reqBody =
          DeleteModelReq {name = modelName}
        request =
          initialRequest
            { method = "DELETE"
            , requestBody = RequestBodyLBS $ encode reqBody
            }
    response <- httpLbs request manager
    when
      (responseStatus response == status404)
      (putStrLn "Model does not exist")
