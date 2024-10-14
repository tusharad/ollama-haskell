{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Common.Utils (defaultOllama, OllamaClient (..)) where

import Data.Ollama.Common.Types

defaultOllama :: OllamaClient
defaultOllama = OllamaClient "http://127.0.0.1:11434"
