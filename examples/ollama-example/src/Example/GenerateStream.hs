{-# LANGUAGE OverloadedStrings #-}

module Example.GenerateStream (runApp) where

import Data.Ollama.Generate
import qualified Data.Text.IO as T
import System.IO (hFlush, stdout)

runApp :: IO ()
runApp = do
  let -- Callback to specify what to do with each response chunk
      streamHandler resp = do
        T.putStr $ genResponse resp
        hFlush stdout
      ops =
        defaultGenerateOps
          { modelName = "gemma3"
          , prompt = "What is the meaning of life?"
          , stream = Just streamHandler
          }
  eRes <- generate ops Nothing
  case eRes of
    Left err -> putStrLn $ "Something went wrong: " ++ show err
    Right _ -> putStrLn "LLM response completed"
