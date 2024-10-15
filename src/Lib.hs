{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Monad
import Data.List.NonEmpty
import Data.Maybe
import Data.Ollama.Chat qualified as Chat
import Data.Text.IO qualified as T
import Ollama (GenerateOps (..), Role (..), chat, defaultChatOps, defaultGenerateOps, generate)
import Ollama qualified

main :: IO ()
main = do
  -- Print the response of streamed data on console.
  void $
    generate
      defaultGenerateOps
        { modelName = "llama3.2"
        , prompt = "what is functional programming?"
        , stream = Just (T.putStr . Ollama.response_, pure ())
        }

  -- Get the non-streamed response from generate function and print it's response
  eRes <-
    generate
      defaultGenerateOps
        { modelName = "llama3.2"
        , prompt = "What is 2+2?"
        }
  case eRes of
    Left e -> putStrLn e
    Right Ollama.GenerateResponse {..} -> T.putStrLn response_

  -- Chat with LLM
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

  eRes1 <-
    chat
      defaultChatOps
        { Chat.chatModelName = "llama3.2"
        , Chat.messages = msg :| []
        , Chat.stream =
            Just (T.putStr . Chat.content . fromMaybe defaultMsg . Chat.message, pure ())
        }

  case eRes1 of
    Left e -> putStrLn e
    Right r -> do
      let mMessage = Ollama.message r
      case mMessage of
        Nothing -> putStrLn "Something went wrong"
        Just res -> T.putStrLn $ Ollama.content res

  -- ps
  res <- Ollama.ps
  print res

  -- Embeddings
  void $ Ollama.embedding "llama3.1" "What is 5+2?"

  -- Embeddings with options
  void $ Ollama.embeddingOps "llama3.1" "What is 5+2?" Nothing Nothing
