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
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Ollama.Types.Common (ModelName)
import Ollama.Types.Message (Message)

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
