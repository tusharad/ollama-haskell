{-# LANGUAGE OverloadedStrings #-}

module Example.Embeddings (runApp) where

import Data.Ollama.Embeddings

runApp :: IO ()
runApp = do
  eRes <- embedding "gemma3" ["Hello World", "Nice to meet you"]
  case eRes of
    Left err -> putStrLn $ "Something went wrong: " ++ show err
    Right r -> do
      putStrLn "LLM response"
      print (respondedEmbeddings r)
