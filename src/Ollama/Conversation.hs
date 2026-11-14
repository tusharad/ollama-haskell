{- |
Module      : Ollama.Conversation
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Conversation store typeclass and in-memory transactional implementation.

@since 1.0.0.0
-}
module Ollama.Conversation (
  Conversation (..),
  ConversationStore (..),
  InMemoryStore (..),
  initInMemoryStore,
  saveConversationInMemory,
  loadConversationInMemory,
  listConversationsInMemory,
  deleteConversationInMemory,

  -- * Autonomous Tool Calling Loop
  RegisteredTool (..),
  ToolLoopConfig (..),
  defaultToolLoopConfig,
  chatWithTools,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Aeson (FromJSON, ToJSON, Value, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (toList)
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Ollama.API.Chat (ChatRequest (..), ChatResponse (..), chat)
import Ollama.Client (OllamaClient)
import Ollama.Error (OllamaError (..))
import Ollama.Types.Common (ModelName)
import Ollama.Types.Message (Message (..), toolResultMessage, toolResultMessageWithId)
import Ollama.Types.Tool (FunctionDef (..), Tool (..), ToolCall (..), ToolCallFunction (..))

{- | Recorded chat conversation session.

@since 1.0.0.0
-}
data Conversation = Conversation
  { conversationId :: !Text
  , messages :: ![Message]
  , model :: !ModelName
  , createdAt :: !UTCTime
  , lastUpdated :: !UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Abstract interface for persisting and managing chat conversations.

@since 1.0.0.0
-}
class (Monad m) => ConversationStore m where
  saveConversation :: Conversation -> m ()
  loadConversation :: Text -> m (Maybe Conversation)
  listConversations :: m [Conversation]
  deleteConversation :: Text -> m Bool

{- | Thread-safe transactional in-memory conversation store.

@since 1.0.0.0
-}
newtype InMemoryStore = InMemoryStore (TVar (Map Text Conversation))

{- | Initialize a new empty 'InMemoryStore'.

@since 1.0.0.0
-}
initInMemoryStore :: IO InMemoryStore
initInMemoryStore = InMemoryStore <$> newTVarIO Map.empty

{- | Save a conversation into an 'InMemoryStore'.

@since 1.0.0.0
-}
saveConversationInMemory :: (MonadIO m) => InMemoryStore -> Conversation -> m ()
saveConversationInMemory (InMemoryStore ref) conv = liftIO $ atomically $ do
  modifyTVar' ref (Map.insert (conversationId conv) conv)

{- | Load a conversation by ID from an 'InMemoryStore'.

@since 1.0.0.0
-}
loadConversationInMemory :: (MonadIO m) => InMemoryStore -> Text -> m (Maybe Conversation)
loadConversationInMemory (InMemoryStore ref) cid = liftIO $ Map.lookup cid <$> readTVarIO ref

{- | List all stored conversations from an 'InMemoryStore'.

@since 1.0.0.0
-}
listConversationsInMemory :: (MonadIO m) => InMemoryStore -> m [Conversation]
listConversationsInMemory (InMemoryStore ref) = liftIO $ Map.elems <$> readTVarIO ref

{- | Delete a conversation by ID from an 'InMemoryStore'.

@since 1.0.0.0
-}
deleteConversationInMemory :: (MonadIO m) => InMemoryStore -> Text -> m Bool
deleteConversationInMemory (InMemoryStore ref) cid = liftIO $ atomically $ do
  m <- readTVar ref
  if Map.member cid m
    then modifyTVar' ref (Map.delete cid) >> pure True
    else pure False

instance (MonadIO m) => ConversationStore (ReaderT InMemoryStore m) where
  saveConversation conv = ask >>= \store -> saveConversationInMemory store conv
  loadConversation cid = ask >>= \store -> loadConversationInMemory store cid
  listConversations = ask >>= \store -> listConversationsInMemory store
  deleteConversation cid = ask >>= \store -> deleteConversationInMemory store cid

{- | A registered callable tool binding a 'Tool' definition to a monadic handler.
The handler receives argument key-value pairs decoded from the model's tool call
and returns either an error 'Text' or a serializable 'Value'.

@since 0.4.3.0
-}
data RegisteredTool m = RegisteredTool
  { rtTool :: !Tool
  , rtHandler :: !(Map Text Value -> m (Either Text Value))
  }

{- | Configuration for the multi-turn autonomous tool execution loop.

@since 0.4.3.0
-}
data ToolLoopConfig = ToolLoopConfig
  { tlcMaxTurns :: !Int
  -- ^ Maximum turn iterations permitted before aborting with 'InvalidRequest' (default: 10).
  , tlcOnTurn :: !(Maybe (Int -> Message -> IO ()))
  -- ^ Optional callback invoked on each assistant turn with the turn number and message.
  }

{- | Default configuration for 'chatWithTools' with 10 max turns and no callback.

@since 0.4.3.0
-}
defaultToolLoopConfig :: ToolLoopConfig
defaultToolLoopConfig =
  ToolLoopConfig
    { tlcMaxTurns = 10
    , tlcOnTurn = Nothing
    }

{- | Autonomous multi-turn tool calling execution loop.

Sends a 'ChatRequest' with registered tools attached. Whenever the model responds
with one or more tool calls, 'chatWithTools' evaluates their corresponding handlers,
formats the results as tool role messages (preserving IDs and correlation), appends
them to the message history, and continues the conversation until the model produces
a final assistant answer without tool calls or until 'tlcMaxTurns' is reached.

@since 0.4.3.0
-}
chatWithTools ::
  (MonadIO m) =>
  OllamaClient ->
  ChatRequest ->
  [RegisteredTool m] ->
  ToolLoopConfig ->
  m (Either OllamaError (Message, [Message]))
chatWithTools client initialReq tools config =
  go 1 (toList (chatMessages initialReq))
  where
    toolDefs = map rtTool tools
    toolMap = Map.fromList [(fnName (toolFunction (rtTool t)), rtHandler t) | t <- tools]

    go turn currentMessages
      | turn > tlcMaxTurns config =
          let errMsg = "Maximum tool loop turns (" <> T.pack (show (tlcMaxTurns config)) <> ") exceeded"
           in pure (Left (InvalidRequest errMsg))
      | otherwise = do
          case nonEmpty currentMessages of
            Nothing -> pure $ Left $ InvalidRequest "Cannot chat with empty message list"
            Just neMessages -> do
              let req =
                    initialReq
                      { chatMessages = neMessages
                      , chatTools = Just toolDefs
                      , chStream = Just False
                      }
              chatRes <- chat client req
              case chatRes of
                Left err -> pure $ Left err
                Right resp ->
                  case crMessage resp of
                    Nothing ->
                      pure $ Left $ DecodeError "No message returned in chat response" ""
                    Just assistantMsg -> do
                      case tlcOnTurn config of
                        Just cb -> liftIO $ cb turn assistantMsg
                        Nothing -> pure ()
                      case messageToolCalls assistantMsg of
                        Nothing ->
                          pure $ Right (assistantMsg, currentMessages ++ [assistantMsg])
                        Just [] ->
                          pure $ Right (assistantMsg, currentMessages ++ [assistantMsg])
                        Just toolCalls -> do
                          toolResultMsgs <- forM toolCalls $ \tc -> do
                            let fn = tcFunction tc
                                fName = tcfName fn
                                args = tcfArguments fn
                                mbCallId = tcId tc
                            resultContent <- case Map.lookup fName toolMap of
                              Nothing ->
                                pure $ "Error: tool '" <> fName <> "' is not available."
                              Just handler -> do
                                hRes <- handler args
                                pure $ case hRes of
                                  Left errText -> "Error: " <> errText
                                  Right val -> TE.decodeUtf8 (BSL.toStrict (encode val))
                            pure $ case mbCallId of
                              Just cid -> toolResultMessageWithId resultContent fName cid
                              Nothing -> toolResultMessage resultContent fName
                          let updatedMessages = currentMessages ++ [assistantMsg] ++ toolResultMsgs
                          go (turn + 1) updatedMessages
