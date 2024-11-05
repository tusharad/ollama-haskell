{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OllamaExamples (main) where

import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)
import Data.Ollama.Chat qualified as Chat
import Data.Text.IO qualified as T
import Ollama (GenerateOps(..), Role(..), chat, defaultChatOps, defaultGenerateOps, generate)
import Ollama qualified

main :: IO ()
main = do
  -- Example 1: Streamed Text Generation
  -- This example demonstrates how to generate text using a model and stream the output directly to the console.
  -- The `stream` option enables processing of each chunk of the response as it arrives.
  void $
    generate
      defaultGenerateOps
        { modelName = "llama3.2"
        , prompt = "what is functional programming?"
        , stream = Just (T.putStr . Ollama.response_, pure ())
        }

  -- Example 2: Non-streamed Text Generation
  -- This example shows how to generate text and handle the complete response.
  -- The result is either an error message or the generated text.
  eRes <-
    generate
      defaultGenerateOps
        { modelName = "llama3.2"
        , prompt = "What is 2+2?"
        }
  case eRes of
    Left e -> putStrLn e
    Right Ollama.GenerateResponse {..} -> T.putStrLn response_

  -- Example 3: Chat with Streaming
  -- This example demonstrates setting up a chat session with streaming enabled.
  -- As messages are received, they are printed to the console.
  let msg = Ollama.Message User "What is functional programming?" Nothing
      defaultMsg = Ollama.Message User "" Nothing
  void $
    chat
      defaultChatOps
        { Chat.chatModelName = "llama3.2"
        , Chat.messages = msg :| []
        , Chat.stream =
            Just (T.putStr . Chat.content . fromMaybe defaultMsg . Chat.message, pure ())
        }

  -- Example 4: Non-streamed Chat
  -- Here, we handle a complete chat response, checking for potential errors.
  eRes1 <-
    chat
      defaultChatOps
        { Chat.chatModelName = "llama3.2"
        , Chat.messages = msg :| []
        }
  case eRes1 of
    Left e -> putStrLn e
    Right r -> do
      let mMessage = Ollama.message r
      case mMessage of
        Nothing -> putStrLn "Something went wrong"
        Just res -> T.putStrLn $ Ollama.content res

  -- Example 5: Check Model Status (ps)
  -- This example checks the status of models using the `ps` function.
  -- It outputs the status or details of the available models.
  res <- Ollama.ps
  print res

  -- Example 6: Simple Embedding
  -- This demonstrates how to request embeddings for a given text using a specific model.
  void $ Ollama.embedding "llama3.1" "What is 5+2?"

  -- Example 7: Embedding with Options
  -- This example uses the `embeddingOps` function, allowing for additional configuration like options and streaming.
  void $ Ollama.embeddingOps "llama3.1" "What is 5+2?" Nothing Nothing

{-
Scotty example:
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple
import Ollama (GenerateOps(..), defaultGenerateOps, generate)
import Data.Maybe (fromRight)

data PromptInput = PromptInput
  { conversation_id :: Int
  , prompt :: Text
  } deriving (Show, Generic)

instance FromJSON PromptInput
instance ToJSON PromptInput

main :: IO ()
main = do
  conn <- open "chat.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS conversation (convo_id INTEGER PRIMARY KEY, convo_title TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS chats (chat_id INTEGER PRIMARY KEY, convo_id INTEGER, role TEXT, message TEXT, FOREIGN KEY(convo_id) REFERENCES conversation(convo_id))"
  
  scotty 3000 $ do
    post "/chat" $ do
      p <- jsonData :: ActionM PromptInput
      let cId = conversation_id p
      let trimmedP = T.dropEnd 3 $ T.drop 3 $ prompt p
      newConvoId <- case cId of
        -1 -> do
          liftIO $ execute conn "INSERT INTO conversation (convo_title) VALUES (?)" (Only ("latest title" :: String))
          [Only convoId] <- liftIO $ query_ conn "SELECT last_insert_rowid()" :: ActionM [Only Int]
          pure convoId
        _ -> pure cId

      liftIO $ execute conn "INSERT INTO chats (convo_id, role, message) VALUES (?, 'user', ?)" (newConvoId, trimmedP)
      
      stream $ \sendChunk flush -> do
        eRes <- generate defaultGenerateOps
                { modelName = "llama3.2"
                , prompt = prompt p
                , stream = Just (sendChunk . T.pack, flush)
                }
        case eRes of
            Left e -> return ()
            Right r -> do
                let res = response_ r
                liftIO $ execute conn "INSERT INTO chats (convo_id, role, message) VALUES (?, 'ai', ?)" (newConvoId, res)
-}
