{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Ollama

main :: IO ()
main = do
  Ollama.generate "llama3.1" "Is haskell a good language?"
