{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Monad
import Data.Text.IO qualified as T
import Ollama (GenerateOps (..), defaultGenerateOps, generate)
import Ollama qualified

main :: IO ()
main = do
  -- Print the response of streamed data on console.
  void $
    generate
      defaultGenerateOps
        { model = "llama3.2"
        , prompt = "what is functional programming?"
        , stream = Just (T.putStr . Ollama.response_, pure ())
        }

  -- Get the non-streamed response from generate function and print it's response
  eRes <-
    generate
      defaultGenerateOps
        { model = "llama3.2"
        , prompt = "What is 2+2?"
        }
  case eRes of
    Left e -> putStrLn e
    Right Ollama.GenerateResponse {..} -> T.putStrLn response_

  -- Chat with LLM
  let msg = Ollama.Message "user" "What is 2+2?" Nothing
  Ollama.chat "llama3.1" [msg]

  -- Embeddings
  void $ Ollama.embedding "llama3.1" "What is 5+2?"

  -- Embeddings with options
  void $ Ollama.embeddingOps "llama3.1" "What is 5+2?" Nothing Nothing
