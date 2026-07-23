-- |
-- Module      : Ollama.Conversation
-- Copyright   : (c) 2024-2026 Tushar Adhatrao
-- License     : MIT
-- Maintainer  : tusharadhatrao@gmail.com
-- Stability   : stable
-- Portability : portable
--
-- Conversation store typeclass and in-memory transactional implementation.
--
-- @since 1.0.0.0
module Ollama.Conversation
  ( Conversation (..)
  , ConversationStore (..)
  , InMemoryStore (..)
  , initInMemoryStore
  ) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Ollama.Types.Common (ModelName)
import Ollama.Types.Message (Message)

-- | Recorded chat conversation session.
--
-- @since 1.0.0.0
data Conversation = Conversation
  { conversationId :: !Text
  , messages :: ![Message]
  , model :: !ModelName
  , createdAt :: !UTCTime
  , lastUpdated :: !UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Abstract interface for persisting and managing chat conversations.
--
-- @since 1.0.0.0
class Monad m => ConversationStore m where
  saveConversation :: Conversation -> m ()
  loadConversation :: Text -> m (Maybe Conversation)
  listConversations :: m [Conversation]
  deleteConversation :: Text -> m Bool

-- | Thread-safe transactional in-memory conversation store.
--
-- @since 1.0.0.0
newtype InMemoryStore = InMemoryStore (TVar (Map Text Conversation))

-- | Initialize a new empty 'InMemoryStore'.
--
-- @since 1.0.0.0
initInMemoryStore :: IO InMemoryStore
initInMemoryStore = InMemoryStore <$> newTVarIO Map.empty
