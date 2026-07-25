module Main (main) where

import Ollama

main :: IO ()
main = do
  client <- defaultClient
  let req = embedRequest "nomic-embed-text" ["Hello world", "Haskell LLM integration"]
  res <- embed client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> putStrLn $ "Generated " <> show (length $ erEmbeddings resp) <> " embeddings."
