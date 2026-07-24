module Main (main) where

import Ollama

main :: IO ()
main = do
  client <- defaultClient
  res <- listModels client
  case res of
    Left err -> putStrLn $ "Error listing models: " <> show err
    Right (ListResponse ms) -> do
      putStrLn "Installed Models:"
      mapM_ (print . modelInfoName) ms
