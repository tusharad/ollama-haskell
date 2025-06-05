{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Ollama.Conversation
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Conversation management for the Ollama client, including storage and retrieval of chat sessions.

This module provides types and functions for managing conversations in the Ollama client. It defines
a 'Conversation' type to represent a chat session, a 'ConversationStore' typeclass for storage operations,
and an in-memory implementation using 'InMemoryStore' and 'ConvoM'. The module supports saving, loading,
listing, and deleting conversations, with thread-safe operations using STM (Software Transactional Memory).

The 'Conversation' type includes metadata such as a unique ID, messages, model name, and timestamps.
The 'ConversationStore' typeclass defines a generic interface for conversation storage, while 'InMemoryStore'
provides a concrete in-memory implementation. The 'ConvoM' monad integrates with 'InMemoryStore' for
monadic operations.

Example:

>>> store <- initInMemoryStore
>>> let conv = Conversation "conv1" [userMessage "Hello!"] "gemma3" <$> getCurrentTime <*> getCurrentTime
>>> runInMemoryConvo store $ saveConversation conv
>>> runInMemoryConvo store $ loadConversation "conv1"
Just (Conversation ...)
-}
module Data.Ollama.Conversation
  ( -- * Conversation Types
    Conversation (..)
  , ConversationStore (..)

    -- * In-Memory Store
  , InMemoryStore (..)
  , ConvoM (..)
  , initInMemoryStore
  , runInMemoryConvo

    -- * Validation
  , validateConversation
  ) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ollama.Common.Types
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)

{- | Represents a chat session with metadata and messages.

Stores a conversation's unique identifier, list of messages, model name, creation time, and last updated time.
-}
data Conversation = Conversation
  { conversationId :: !Text
  -- ^ Unique identifier for the conversation.
  , messages :: ![Message]
  -- ^ List of messages in the conversation.
  , model :: !Text
  -- ^ Name of the model used in the conversation (e.g., "gemma3").
  , createdAt :: !UTCTime
  -- ^ Timestamp when the conversation was created.
  , lastUpdated :: !UTCTime
  -- ^ Timestamp when the conversation was last updated.
  }
  deriving (Show, Eq, Generic)

instance ToJSON Conversation
instance FromJSON Conversation

{- | Typeclass defining operations for storing and managing conversations.

Provides methods for saving, loading, listing, and deleting conversations in a monadic context.
--
-- @since 0.2.0.0
-}
class Monad m => ConversationStore m where
  -- | Saves a conversation to the store.
  --
  -- Validates the conversation and updates its 'lastUpdated' timestamp before saving.
  saveConversation :: Conversation -> m ()

  -- | Loads a conversation by its ID.
  --
  -- Returns 'Just' the conversation if found, or 'Nothing' if not.
  loadConversation :: Text -> m (Maybe Conversation)

  -- | Lists all conversations in the store.
  listConversations :: m [Conversation]

  -- | Deletes a conversation by its ID.
  --
  -- Returns 'True' if the conversation was found and deleted, 'False' otherwise.
  deleteConversation :: Text -> m Bool

{- | In-memory conversation store using a 'TVar' for thread-safe operations.

Stores conversations in a 'Map' keyed by conversation IDs, wrapped in a 'TVar' for concurrent access.
-}
newtype InMemoryStore = InMemoryStore (TVar (Map Text Conversation))

{- | Monad for operations with 'InMemoryStore'.

A wrapper around 'ReaderT' that provides access to an 'InMemoryStore' in a monadic context.
-}
newtype ConvoM a = ConvoM {runConvoM :: ReaderT InMemoryStore IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader InMemoryStore)

{- | Runs a 'ConvoM' action with the given 'InMemoryStore'.

Executes a monadic computation in the context of an in-memory store.

Example:

>>> store <- initInMemoryStore
>>> runInMemoryConvo store $ saveConversation conv
-}
runInMemoryConvo :: InMemoryStore -> ConvoM a -> IO a
runInMemoryConvo store = flip runReaderT store . runConvoM

instance ConversationStore ConvoM where
  saveConversation conv = do
    case validateConversation conv of
      Left err -> liftIO $ putStrLn ("Validation error: " <> T.unpack err)
      Right validConv -> do
        now <- liftIO getCurrentTime
        let updatedConv = validConv {lastUpdated = now}
        InMemoryStore ref <- ask
        liftIO . atomically $ modifyTVar' ref (Map.insert (conversationId updatedConv) updatedConv)

  loadConversation cid = do
    InMemoryStore ref <- ask
    convs <- liftIO $ readTVarIO ref
    return $ Map.lookup cid convs

  listConversations = do
    InMemoryStore ref <- ask
    convs <- liftIO $ readTVarIO ref
    return $ Map.elems convs

  deleteConversation cid = do
    InMemoryStore ref <- ask
    liftIO . atomically $ do
      convs <- readTVar ref
      if Map.member cid convs
        then do
          writeTVar ref (Map.delete cid convs)
          return True
        else return False

{- | Validates a 'Conversation' to ensure required fields are non-empty.

Checks that the 'conversationId' is not empty and that the 'messages' list contains at least one message.
Returns 'Right' with the validated conversation or 'Left' with an error message.

Example:

>>> let conv = Conversation "" [] "gemma3" time time
>>> validateConversation conv
Left "Conversation ID cannot be empty"
-}
validateConversation :: Conversation -> Either Text Conversation
validateConversation conv
  | T.null (conversationId conv) = Left "Conversation ID cannot be empty"
  | null (messages conv) = Left "Conversation must have at least one message"
  | otherwise = Right conv

{- | Creates a new empty in-memory conversation store.

Initializes an 'InMemoryStore' with an empty 'Map' wrapped in a 'TVar' for thread-safe operations.

Example:

>>> store <- initInMemoryStore
>>> runInMemoryConvo store $ listConversations
[]
-}
initInMemoryStore :: IO InMemoryStore
initInMemoryStore = InMemoryStore <$> newTVarIO Map.empty
