{- |
Module      : Ollama.Testing
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Mock client and test helpers for unit testing code without a live Ollama server.

@since 1.0.0.0
-}
module Ollama.Testing (
  newMockClient,
  withMockClient,
  mockGenerateResponse,
  mockChatResponse,
  mockEmbedResponse,
  mockListModelsResponse,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef (atomicModifyIORef, newIORef)
import Data.Text (Text)
import Data.Time (Day (..), UTCTime (..))
import Network.HTTP.Client (
  defaultManagerSettings,
  managerRawConnection,
  newManager,
 )
import Network.HTTP.Client.Internal (makeConnection)
import Ollama.API.Chat (ChatResponse (..))
import Ollama.API.Embed (EmbedResponse (..))
import Ollama.API.Generate (GenerateResponse (..))
import Ollama.Client (OllamaClient (..), closeClient)
import Ollama.Client.Config (defaultConfig)
import Ollama.Types.Common (Digest (..), Duration (..), ModelName (..))
import Ollama.Types.Message (assistantMessage)
import Ollama.Types.Model (ListResponse (..), ModelDetails (..), ModelInfo (..))

{- | Construct an 'OllamaClient' that returns mock HTTP response bytes for any request.

@since 1.0.0.0
-}
newMockClient :: (MonadIO m) => ByteString -> m OllamaClient
newMockClient bodyBytes = liftIO $ do
  let httpResponse =
        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: "
          <> BS8.pack (show (BS8.length bodyBytes))
          <> "\r\n\r\n"
          <> bodyBytes
  ref <- newIORef [httpResponse]
  let mockConn =
        makeConnection
          ( atomicModifyIORef ref $ \case
              [] -> ([], "")
              (x : xs) -> (xs, x)
          )
          (\_ -> pure ())
          (pure ())
      settings = defaultManagerSettings {managerRawConnection = pure (\_ _ _ -> mockConn)}
  mgr <- newManager settings
  pure $ OllamaClient mgr defaultConfig True

{- | Resource bracket helper to create a mock client, execute an action, and close resources.

@since 1.0.0.0
-}
withMockClient :: (MonadUnliftIO m) => ByteString -> (OllamaClient -> m a) -> m a
withMockClient body action = do
  client <- newMockClient body
  res <- action client
  closeClient client
  pure res

{- | Construct a mock 'GenerateResponse'.

@since 1.0.0.0
-}
mockGenerateResponse :: ModelName -> Text -> GenerateResponse
mockGenerateResponse model text =
  GenerateResponse
    { grModel = model
    , grCreatedAt = UTCTime (ModifiedJulianDay 60000) 0
    , grResponse = text
    , grDone = True
    , grDoneReason = Just "stop"
    , grContext = Just [1, 2, 3]
    , grTotalDuration = Just (Duration 1000000000)
    , grLoadDuration = Just (Duration 100000000)
    , grPromptEvalCount = Just 10
    , grPromptEvalDuration = Just (Duration 200000000)
    , grEvalCount = Just 20
    , grEvalDuration = Just (Duration 700000000)
    , grThinking = Nothing
    , grImage = Nothing
    }

{- | Construct a mock 'ChatResponse'.

@since 1.0.0.0
-}
mockChatResponse :: ModelName -> Text -> ChatResponse
mockChatResponse model text =
  ChatResponse
    { crModel = model
    , crCreatedAt = UTCTime (ModifiedJulianDay 60000) 0
    , crMessage = Just (assistantMessage text)
    , crDone = True
    , crDoneReason = Just "stop"
    , crTotalDuration = Just (Duration 1000000000)
    , crLoadDuration = Just (Duration 100000000)
    , crPromptEvalCount = Just 10
    , crPromptEvalDuration = Just (Duration 200000000)
    , crEvalCount = Just 20
    , crEvalDuration = Just (Duration 700000000)
    }

{- | Construct a mock 'EmbedResponse'.

@since 1.0.0.0
-}
mockEmbedResponse :: ModelName -> [[Double]] -> EmbedResponse
mockEmbedResponse model vectors =
  EmbedResponse
    { erModel = model
    , erEmbeddings = vectors
    , erTotalDuration = Just (Duration 500000000)
    , erLoadDuration = Just (Duration 50000000)
    , erPromptEvalCount = Just 8
    }

{- | Construct a mock 'ListResponse'.

@since 1.0.0.0
-}
mockListModelsResponse :: [ModelName] -> ListResponse
mockListModelsResponse names =
  ListResponse
    { models = map mkModel names
    }
  where
    mkModel name =
      ModelInfo
        { miName = name
        , miModel = name
        , miModifiedAt = UTCTime (ModifiedJulianDay 60000) 0
        , miSize = 4000000000
        , miDigest = Digest "sha256:1234567890abcdef"
        , miDetails =
            ModelDetails
              { parentModel = Nothing
              , format = "gguf"
              , family = "llama"
              , families = ["llama"]
              , parameterSize = "7B"
              , quantizationLevel = "Q4_K_M"
              }
        }
