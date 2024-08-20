{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Ollama

main :: IO ()
main = do
    generate "llama3.1" "Is haskell a good language?"