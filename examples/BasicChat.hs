module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  let req = chatRequest "llama3.2" (userMessage "Why is the sky blue?" :| [])
  res <- chat client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> case crMessage resp of
      Just msg -> putStrLn $ "Response: " <> messageContent msg
      Nothing -> putStrLn "No content returned"
