{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Copy
  ( -- * Copy Model API
    copyModel
  ) where

import Control.Monad (when)
import Data.Aeson
import Data.Ollama.Common.Utils qualified as CU
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types.Status (status404)

-- TODO: Add Options parameter
-- TODO: Add Context parameter
data CopyModelOps = CopyModelOps
  { source :: Text
  , destination :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

-- | Copy model from source to destination
copyModel ::
  -- | Source model
  Text ->
  -- | Destination model
  Text ->
  IO ()
copyModel
  source_
  destination_ =
    do
      let url = CU.defaultOllamaUrl
      manager <- newManager defaultManagerSettings
      initialRequest <- parseRequest $ T.unpack (url <> "/api/copy")
      let reqBody =
            CopyModelOps
              { source = source_
              , destination = destination_
              }
          request =
            initialRequest
              { method = "POST"
              , requestBody = RequestBodyLBS $ encode reqBody
              }
      response <- httpLbs request manager
      when
        (responseStatus response == status404)
        (putStrLn "Source Model does not exist")
