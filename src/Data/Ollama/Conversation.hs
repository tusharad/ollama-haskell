{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Conversation (
    Conversation (..)
  , ConversationStore (..)
  , InMemoryStore (..)
  , ConvoM (..)
  , validateConversation
  , initInMemoryStore
  , runInMemoryConvo
 ) where

import Control.Concurrent.STM
import Data.Ollama.Common.Types
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Control.Monad.Reader

-- Main Conversation type
data Conversation = Conversation
  { conversationId :: !Text
  , messages :: ![Message]
  , model :: !Text
  , createdAt :: !UTCTime
  , lastUpdated :: !UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Conversation
instance FromJSON Conversation

-- Store interface
class Monad m => ConversationStore m where
  saveConversation :: Conversation -> m ()
  loadConversation :: Text -> m (Maybe Conversation)
  listConversations :: m [Conversation]
  deleteConversation :: Text -> m Bool

-- | In-memory store with TVar for concurrency
newtype InMemoryStore = InMemoryStore (TVar (Map Text Conversation))

newtype ConvoM a = ConvoM { runConvoM :: ReaderT InMemoryStore IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader InMemoryStore)

runInMemoryConvo :: InMemoryStore -> ConvoM a -> IO a
runInMemoryConvo store = flip runReaderT store . runConvoM

instance ConversationStore ConvoM where
  saveConversation conv = do
    case validateConversation conv of
      Left err -> liftIO $ putStrLn ("Validation error: " <> T.unpack err)
      Right validConv -> do
        now <- liftIO getCurrentTime
        let updatedConv = validConv { lastUpdated = now }
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

-- Validation for Conversations
validateConversation :: Conversation -> Either Text Conversation
validateConversation conv
  | T.null (conversationId conv) = Left "Conversation ID cannot be empty"
  | null (messages conv) = Left "Conversation must have at least one message"
  | otherwise = Right conv

-- Utility to create a new store
initInMemoryStore :: IO InMemoryStore
initInMemoryStore = InMemoryStore <$> newTVarIO Map.empty
