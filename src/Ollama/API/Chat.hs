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
  StructuredResponse (..),
  chatRequest,
  chat,
  chatStructured,
  chatStream,
  accumulateChat,
  accumulateChatChunks,
  accumulateChatStream,
  chatEvalTokensPerSecond,
  chatPromptEvalTokensPerSecond,
) where

import Conduit (ConduitT)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (request, requestStreaming)
import Ollama.Error (OllamaError (..))
import Ollama.Streaming (HasDone (..), accumulateStream)
import Ollama.Types.Common (Duration, Logprob, ModelName (..), Think, tokensPerSecond)
import Ollama.Types.Format (Format, ToSchema, formatFor)
import Ollama.Types.Message (Message (..))
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
  , chStream :: !(Maybe Bool)
  , chatKeepAlive :: !(Maybe Text)
  , chatThink :: !(Maybe Think)
  , chatLogprobs :: !(Maybe Bool)
  , chatTopLogprobs :: !(Maybe Int)
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
        , ("stream" .=) <$> chStream
        , ("keep_alive" .=) <$> chatKeepAlive
        , ("think" .=) <$> chatThink
        , ("logprobs" .=) <$> chatLogprobs
        , ("top_logprobs" .=) <$> chatTopLogprobs
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
    , chStream = Just False
    , chatKeepAlive = Nothing
    , chatThink = Nothing
    , chatLogprobs = Nothing
    , chatTopLogprobs = Nothing
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
  , crLogprobs :: !(Maybe [Logprob])
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
      <*> v .:? "logprobs"

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
      , "logprobs" .= crLogprobs
      ]

instance HasDone ChatResponse where
  isDone = crDone

{- | Non-streaming chat completion API.

@since 1.0.0.0
-}
chat :: (MonadIO m) => OllamaClient -> ChatRequest -> m (Either OllamaError ChatResponse)
chat client req = request client "POST" "/api/chat" (Just req {chStream = Just False})

{- | A structured response containing both the typed decoded payload and the raw 'ChatResponse'.

@since 0.4.3.0
-}
data StructuredResponse a = StructuredResponse
  { srValue :: !a
  , srRawResponse :: !ChatResponse
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Perform a chat completion enforcing a JSON Schema derived from the type parameter @a@ via 'ToSchema',
and automatically decode the assistant message content into @a@.

If the model response fails to decode into @a@, returns 'DecodeError' containing the parse error and raw bytes.

@since 0.4.3.0
-}
chatStructured ::
  forall a m.
  (MonadIO m, FromJSON a, ToSchema a) =>
  OllamaClient ->
  ChatRequest ->
  m (Either OllamaError (StructuredResponse a))
chatStructured client req = do
  let structuredReq = req {chatFormat = Just (formatFor @a), chStream = Just False}
  res <- chat client structuredReq
  pure $ case res of
    Left err -> Left err
    Right raw ->
      case crMessage raw of
        Nothing -> Left (DecodeError "No message returned in chat response" "")
        Just msg ->
          let txt = messageContent msg
              bs = TE.encodeUtf8 txt
           in case eitherDecode (BSL.fromStrict bs) of
                Left parseErr -> Left (DecodeError (T.pack parseErr) bs)
                Right val -> Right (StructuredResponse val raw)

{- | Streaming chat completion API yielding 'ChatResponse' chunks.

@since 1.0.0.0
-}
chatStream :: (MonadUnliftIO m) => OllamaClient -> ChatRequest -> ConduitT () ChatResponse m ()
chatStream client req = requestStreaming client "/api/chat" (req {chStream = Just True})

{- | Calculate generation throughput (eval tokens \/ second) from a 'ChatResponse'.

@since 1.0.0.0
-}
chatEvalTokensPerSecond :: ChatResponse -> Maybe Double
chatEvalTokensPerSecond ChatResponse {..} =
  tokensPerSecond <$> crEvalCount <*> crEvalDuration

{- | Calculate prompt evaluation throughput (prompt tokens \/ second) from a 'ChatResponse'.

@since 1.0.0.0
-}
chatPromptEvalTokensPerSecond :: ChatResponse -> Maybe Double
chatPromptEvalTokensPerSecond ChatResponse {..} =
  tokensPerSecond <$> crPromptEvalCount <*> crPromptEvalDuration

{- | Combine two consecutive 'ChatResponse' chunks into an accumulated response.
Sequential text and thinking tokens are concatenated, tool calls are preserved in order,
and durations and completion flags from the latest chunk are maintained.

@since 0.4.3.0
-}
accumulateChat :: ChatResponse -> ChatResponse -> ChatResponse
accumulateChat r1 r2 =
  r2
    { crModel = if T.null (unModelName (crModel r1)) then crModel r2 else crModel r1
    , crCreatedAt = crCreatedAt r1
    , crMessage = combineMessages (crMessage r1) (crMessage r2)
    , crDone = crDone r1 || crDone r2
    , crDoneReason = crDoneReason r2 <|> crDoneReason r1
    , crTotalDuration = crTotalDuration r2 <|> crTotalDuration r1
    , crLoadDuration = crLoadDuration r2 <|> crLoadDuration r1
    , crPromptEvalCount = crPromptEvalCount r2 <|> crPromptEvalCount r1
    , crPromptEvalDuration = crPromptEvalDuration r2 <|> crPromptEvalDuration r1
    , crEvalCount = crEvalCount r2 <|> crEvalCount r1
    , crEvalDuration = crEvalDuration r2 <|> crEvalDuration r1
    , crLogprobs = combineLogprobs (crLogprobs r1) (crLogprobs r2)
    }
  where
    combineMessages Nothing m2 = m2
    combineMessages m1 Nothing = m1
    combineMessages (Just m1) (Just m2) =
      Just $
        m2
          { messageRole = messageRole m1
          , messageContent = messageContent m1 <> messageContent m2
          , messageImages = combineLists (messageImages m1) (messageImages m2)
          , messageToolCalls = combineLists (messageToolCalls m1) (messageToolCalls m2)
          , messageToolName = messageToolName m2 <|> messageToolName m1
          , messageThinking = combineText (messageThinking m1) (messageThinking m2)
          , messageToolCallId = messageToolCallId m2 <|> messageToolCallId m1
          }

    combineText Nothing t2 = t2
    combineText t1 Nothing = t1
    combineText (Just t1) (Just t2) = Just (t1 <> t2)

    combineLists Nothing l2 = l2
    combineLists l1 Nothing = l1
    combineLists (Just l1) (Just l2) = Just (l1 ++ l2)

    combineLogprobs Nothing lp2 = lp2
    combineLogprobs lp1 Nothing = lp1
    combineLogprobs (Just lp1) (Just lp2) = Just (lp1 ++ lp2)

{- | Accumulate a list of 'ChatResponse' chunks into a single unified 'ChatResponse'.
Returns 'InvalidRequest' if the input list is empty.

@since 0.4.3.0
-}
accumulateChatChunks :: [ChatResponse] -> Either OllamaError ChatResponse
accumulateChatChunks [] = Left (InvalidRequest "Cannot accumulate empty stream of ChatResponse")
accumulateChatChunks (c : cs) = Right (go c cs)
  where
    go acc [] = acc
    go acc (x : xs) =
      let acc' = accumulateChat acc x
       in acc' `seq` go acc' xs

{- | Fold over a streaming chat conduit and accumulate all emitted chunks into a single
final 'ChatResponse'.

@since 0.4.3.0
-}
accumulateChatStream ::
  (MonadUnliftIO m) =>
  ConduitT () ChatResponse m () ->
  m (Either OllamaError ChatResponse)
accumulateChatStream = accumulateStream accumulateChatChunks
