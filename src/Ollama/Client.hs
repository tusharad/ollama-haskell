{- |
Module      : Ollama.Client
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Opaque client management and construction functions.

@since 1.0.0.0
-}
module Ollama.Client (
  OllamaClient (..),
  newClient,
  defaultClient,
  clientFromEnv,
  closeClient,
  withClient,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client (Manager, closeManager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Ollama.Client.Config (OllamaClientConfig (..), defaultConfig)
import System.Environment (lookupEnv)

{- | Opaque client handle for dispatching API calls.

@since 1.0.0.0
-}
data OllamaClient = OllamaClient
  { clientManager :: !Manager
  , clientConfig :: !OllamaClientConfig
  , clientOwned :: !Bool
  }

{- | Construct a client with custom configuration settings.

@since 1.0.0.0
-}
newClient :: (MonadIO m) => OllamaClientConfig -> m OllamaClient
newClient cfg = liftIO $ do
  case configManager cfg of
    Just mgr -> pure $ OllamaClient mgr cfg False
    Nothing -> do
      mgr <- newManager tlsManagerSettings
      pure $ OllamaClient mgr cfg True

{- | Construct a client with default settings (@http://127.0.0.1:11434@).

@since 1.0.0.0
-}
defaultClient :: (MonadIO m) => m OllamaClient
defaultClient = newClient defaultConfig

{- | Construct a client resolving host and credentials from environment variables
(@OLLAMA_HOST@, @OLLAMA_API_KEY@).

Parses @OLLAMA_HOST@ robustly, handling these formats:

  * @host:port@        → @http:\/\/host:port@
  * @http:\/\/host:port@ → used as-is
  * @host@             → @http:\/\/host:11434@

@since 1.0.0.0
-}
clientFromEnv :: (MonadIO m) => m OllamaClient
clientFromEnv = liftIO $ do
  mbHost <- lookupEnv "OLLAMA_HOST"
  mbKey <- lookupEnv "OLLAMA_API_KEY"
  let hostText = maybe "http://127.0.0.1:11434" (normalizeHost . T.pack) mbHost
      keyText = T.pack <$> mbKey
      cfg = defaultConfig {configBaseUrl = hostText, configApiKey = keyText}
  newClient cfg

{- | Normalize an @OLLAMA_HOST@ value to a full URL with scheme.

Handles: bare @host:port@, @http(s):\/\/host:port@, and bare @host@.

@since 1.0.0.0
-}
normalizeHost :: Text -> Text
normalizeHost raw
  | "http://" `T.isPrefixOf` raw || "https://" `T.isPrefixOf` raw = T.dropWhileEnd (== '/') raw
  | ":" `T.isInfixOf` raw = "http://" <> T.dropWhileEnd (== '/') raw
  | otherwise = "http://" <> T.dropWhileEnd (== '/') raw <> ":11434"

{- | Close the underlying HTTP connection manager if owned by this client.

@since 1.0.0.0
-}
closeClient :: (MonadIO m) => OllamaClient -> m ()
closeClient client = liftIO $ when (clientOwned client) $ closeManager (clientManager client)

{- | Resource bracket helper to initialize, run a computation, and close client resources.

@since 1.0.0.0
-}
withClient :: (MonadUnliftIO m) => OllamaClientConfig -> (OllamaClient -> m a) -> m a
withClient cfg action = withRunInIO $ \run -> do
  client <- newClient cfg
  res <- run (action client)
  closeClient client
  pure res
