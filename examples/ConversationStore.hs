module Main (main) where

import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Ollama

main :: IO ()
main = do
  putStrLn "=== STM In-Memory Conversation Store Showcase ==="

  putStrLn "\n1. Initializing in-memory store (initInMemoryStore)..."
  store <- initInMemoryStore

  now <- getCurrentTime
  let conv1 =
        Conversation
          { conversationId = "sess-alpha-1"
          , messages =
              [ systemMessage "You are an AI coding assistant."
              , userMessage "What is an applicative functor?"
              , assistantMessage "An applicative functor allows applying functions within a context."
              ]
          , model = "llama3.2:latest"
          , createdAt = now
          , lastUpdated = now
          }

      conv2 =
        Conversation
          { conversationId = "sess-beta-2"
          , messages = [userMessage "Hello world"]
          , model = "qwen3.5:2b"
          , createdAt = now
          , lastUpdated = now
          }

  putStrLn "\n2. Saving conversations (saveConversationInMemory)..."
  saveConversationInMemory store conv1
  saveConversationInMemory store conv2
  putStrLn "Saved sessions: 'sess-alpha-1' and 'sess-beta-2'."

  putStrLn "\n3. Listing conversations (listConversationsInMemory)..."
  allConvs <- listConversationsInMemory store
  putStrLn $ "Total active conversations: " <> show (length allConvs)
  mapM_ (\c -> putStrLn $ "  - ID: " <> T.unpack (conversationId c) <> " | Messages: " <> show (length (messages c)) <> " | Model: " <> show (unModelName (model c))) allConvs

  putStrLn "\n4. Loading specific conversation (loadConversationInMemory)..."
  loaded <- loadConversationInMemory store "sess-alpha-1"
  case loaded of
    Nothing -> putStrLn "Error: failed to load conversation."
    Just c -> do
      putStrLn $ "Successfully loaded " <> T.unpack (conversationId c) <> ":"
      mapM_ (\m -> putStrLn $ "    [" <> show (messageRole m) <> "] " <> T.unpack (messageContent m)) (messages c)

  putStrLn "\n5. Deleting conversation (deleteConversationInMemory)..."
  _ <- deleteConversationInMemory store "sess-beta-2"
  remaining <- listConversationsInMemory store
  putStrLn $ "Remaining conversations after deletion: " <> show (length remaining)
  mapM_ (\c -> putStrLn $ "  - ID: " <> T.unpack (conversationId c)) remaining
