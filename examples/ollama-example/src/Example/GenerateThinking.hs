{-# LANGUAGE OverloadedStrings #-}

module Example.GenerateThinking (runApp) where

import Data.Maybe (fromMaybe)
import Data.Ollama.Generate
import qualified Data.Text.IO as T

runApp :: IO ()
runApp = do
  let ops =
        defaultGenerateOps
          { modelName = "qwen3.5:2b"
          , prompt = "Why is sky blue?"
          , think = Just True
          }
  eRes <- generate ops Nothing
  case eRes of
    Left err -> putStrLn $ "Something went wrong: " ++ show err
    Right r -> do
      putStrLn "LLM response"
      T.putStrLn (genResponse r)
      putStrLn "LLM thinking"
      T.putStrLn (fromMaybe "" (thinking r))
