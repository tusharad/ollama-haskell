{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Copy
  ( -- * Copy Model API
    copyModelOps
  , copyModel
  ) where

import Control.Monad (when)
import Data.Aeson
import Data.Ollama.Common.Utils qualified as CU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (fromMaybe)
import GHC.Generics
import Network.HTTP.Client.TLS
import Network.HTTP.Client
import Network.HTTP.Types.Status (status404)
import Data.Ollama.Common.Config (OllamaConfig (..))

-- TODO: Add Options parameter
-- TODO: Add Context parameter
data CopyModelOps = CopyModelOps
  { source :: !Text
  , destination :: !Text
  }
  deriving (Show, Eq, Generic, ToJSON)


copyModel :: Text -> Text -> IO ()
copyModel src dest = copyModelOps src dest Nothing


-- | Copy model from source to destination
copyModelOps ::
  -- | Source model
  Text ->
  -- | Destination model
  Text ->
  -- | Ollama config
  Maybe OllamaConfig ->
  IO ()
copyModelOps
  source_
  destination_ 
  mbOllamaConfig =
    do
      let url = fromMaybe CU.defaultOllamaUrl (hostUrl <$> mbOllamaConfig)
          timeoutMicros = let t = maybe 15 timeout mbOllamaConfig in t * 60 * 1000000
      manager <- newTlsManagerWith defaultManagerSettings
                { managerResponseTimeout = responseTimeoutMicro timeoutMicros}
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
      maybe (pure ()) (mapM_ id . onModelStart) mbOllamaConfig
      response <- httpLbs request manager
      maybe (pure ()) (mapM_ id . onModelFinish) mbOllamaConfig
      when
        (responseStatus response == status404)
        (do 
          maybe (pure ()) (mapM_ id . onModelError) mbOllamaConfig 
          putStrLn "Source Model does not exist"
        )
