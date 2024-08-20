{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Delete (deleteModel) where

import Control.Monad (when)
import Data.Aeson
import qualified Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Types.Status (status404)

-- TODO: Add Options parameter
-- TODO: Add Context parameter
newtype DeleteModelReq = DeleteModelReq { name :: Text }
  deriving newtype (Show, Eq, ToJSON)

deleteModel ::
  Text ->
  IO ()
deleteModel modelName =
  do
    let url = CU.host CU.defaultOllama
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest $ T.unpack (url <> "/api/delete")
    let reqBody =
          DeleteModelReq {name = modelName}
        request =
          initialRequest
            { method = "DELETE",
              requestBody = RequestBodyLBS $ encode reqBody
            }
    response <- httpLbs request manager
    when
      (responseStatus response == status404)
      (putStrLn "Model does not exist")
