module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  let req = chatRequest "llama3.2" (userMessage "Count from 1 to 5." :| [])
  chunks <- collectStream (chatStream client req)
  mapM_ (putStrLn . maybe "" messageContent . crMessage) chunks
