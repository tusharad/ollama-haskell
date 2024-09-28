{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Ollama

main :: IO ()
main = do
  -- Generate text
  Ollama.generate "llama3.1" "What is 5+2?"

  -- Chat with LLM
  let msg = Ollama.Message "user" "What is 2+2?" Nothing
  Ollama.chat "llama3.1" [msg]

 -- Chat with LLM with options
  Ollama.chatOps "llama3.1" [msg] Nothing Nothing Nothing Nothing

  -- Embeddings
  mRes1 <- Ollama.embedding "llama3.1" "What is 5+2?"

  -- Embeddings with options
  mRes2 <- Ollama.embeddingOps "llama3.1" "What is 5+2?" Nothing Nothing

  Ollama.generateOps "llama3.1" "What is 5+2?" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

  let mRes3 = Ollama.generateReturningResponse "llama3.1" "What is 5+2?"  
  pure ()
