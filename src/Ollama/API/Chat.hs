{- |
Module      : Ollama.API.Chat
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Chat completion API endpoint (@/api/chat@).

@since 1.0.0.0
-}
module Ollama.API.Chat (
  ChatRequest (..),
  ChatResponse (..),
  chatRequest,
  chat,
  chatStream,
) where

import Conduit (ConduitT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (request)
import Ollama.Error (OllamaError)
import Ollama.Streaming (HasDone (..))
import Ollama.Types.Common (Duration, ModelName, Think)
import Ollama.Types.Format (Format)
import Ollama.Types.Message (Message)
import Ollama.Types.Options (ModelOptions)
import Ollama.Types.Tool (Tool)

{- | Chat completion request payload.

@since 1.0.0.0
-}
data ChatRequest = ChatRequest
  { chatModel :: !ModelName
  , chatMessages :: !(NonEmpty Message)
  , chatTools :: !(Maybe [Tool])
  , chatFormat :: !(Maybe Format)
  , chatOptions :: !(Maybe ModelOptions)
  , crqStream :: !(Maybe Bool)
  , chatKeepAlive :: !(Maybe Text)
  , chatThink :: !(Maybe Think)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ChatRequest where
  toJSON ChatRequest {..} =
    object $
      catMaybes
        [ Just $ "model" .= chatModel
        , Just $ "messages" .= chatMessages
        , ("tools" .=) <$> chatTools
        , ("format" .=) <$> chatFormat
        , ("options" .=) <$> chatOptions
        , ("stream" .=) <$> crqStream
        , ("keep_alive" .=) <$> chatKeepAlive
        , ("think" .=) <$> chatThink
        ]

{- | Create a default 'ChatRequest' for a model and message history.

@since 1.0.0.0
-}
chatRequest :: ModelName -> NonEmpty Message -> ChatRequest
chatRequest model msgs =
  ChatRequest
    { chatModel = model
    , chatMessages = msgs
    , chatTools = Nothing
    , chatFormat = Nothing
    , chatOptions = Nothing
    , crqStream = Just False
    , chatKeepAlive = Nothing
    , chatThink = Nothing
    }

{- | Chat completion response payload.

@since 1.0.0.0
-}
data ChatResponse = ChatResponse
  { crModel :: !ModelName
  , crCreatedAt :: !UTCTime
  , crMessage :: !(Maybe Message)
  , crDone :: !Bool
  , crDoneReason :: !(Maybe Text)
  , crTotalDuration :: !(Maybe Duration)
  , crLoadDuration :: !(Maybe Duration)
  , crPromptEvalCount :: !(Maybe Int)
  , crPromptEvalDuration :: !(Maybe Duration)
  , crEvalCount :: !(Maybe Int)
  , crEvalDuration :: !(Maybe Duration)
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ChatResponse where
  parseJSON = withObject "ChatResponse" $ \v ->
    ChatResponse
      <$> v .: "model"
      <*> v .: "created_at"
      <*> v .:? "message"
      <*> v .: "done"
      <*> v .:? "done_reason"
      <*> v .:? "total_duration"
      <*> v .:? "load_duration"
      <*> v .:? "prompt_eval_count"
      <*> v .:? "prompt_eval_duration"
      <*> v .:? "eval_count"
      <*> v .:? "eval_duration"

instance ToJSON ChatResponse where
  toJSON ChatResponse {..} =
    object
      [ "model" .= crModel
      , "created_at" .= crCreatedAt
      , "message" .= crMessage
      , "done" .= crDone
      , "done_reason" .= crDoneReason
      , "total_duration" .= crTotalDuration
      , "load_duration" .= crLoadDuration
      , "prompt_eval_count" .= crPromptEvalCount
      , "prompt_eval_duration" .= crPromptEvalDuration
      , "eval_count" .= crEvalCount
      , "eval_duration" .= crEvalDuration
      ]

instance HasDone ChatResponse where
  isDone = crDone

{- | Non-streaming chat completion API.

@since 1.0.0.0
-}
chat :: (MonadIO m) => OllamaClient -> ChatRequest -> m (Either OllamaError ChatResponse)
chat client req = request client "POST" "/api/chat" (Just req {crqStream = Just False})

{- | Streaming chat completion API.

@since 1.0.0.0
-}
chatStream :: (MonadIO m) => OllamaClient -> ChatRequest -> ConduitT () ChatResponse m ()
chatStream _client _req = liftIO $ pure ()
