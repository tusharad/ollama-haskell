{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad
import Ollama qualified

main :: IO ()
main = do
  -- Generate text
  Ollama.generate "llama3.1" "What is 5+2?"

  -- Chat with LLM
  let msg = Ollama.Message "user" "What is 2+2?" Nothing
  Ollama.chat "llama3.1" [msg]
  -- Embeddings
  void $ Ollama.embedding "llama3.1" "What is 5+2?"

  -- Embeddings with options
  void $ Ollama.embeddingOps "llama3.1" "What is 5+2?" Nothing Nothing
