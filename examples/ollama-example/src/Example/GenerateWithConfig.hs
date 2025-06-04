{-# LANGUAGE OverloadedStrings #-}

module Example.GenerateWithConfig (runApp) where

import Data.Ollama.Generate
import qualified Data.Text.IO as T

runApp :: IO ()
runApp = do
  let ops =
        defaultGenerateOps
          { modelName = "gemma3"
          , prompt = "What is the meaning of life?"
          }
  let ollamaCfg =
        defaultOllamaConfig
          { timeout = 120 -- LLMs usally takes a lot of time for generation;
          -- this sets the timeout time for LLM response (in seconds); default 90 seconds
          , retryCount = Just 2 -- Retry 2 times after failing
          , retryDelay = Just 2 -- Wait 2 seconds before retrying
          }
  eRes <- generate ops (Just ollamaCfg)
  case eRes of
    Left err -> putStrLn $ "Something went wrong: " ++ show err
    Right r -> do
      putStrLn "LLM response"
      T.putStrLn (genResponse r)
