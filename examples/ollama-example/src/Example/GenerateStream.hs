{-# LANGUAGE OverloadedStrings #-}

module Example.GenerateStream (runApp) where

import Data.Ollama.Generate
import qualified Data.Text.IO as T

-- Pass callback function to define what you wanna do with those chunks

runApp :: IO ()
runApp = do
  let streamHandler = (T.putStr . genResponse, pure ())
  let ops =
        defaultGenerateOps
          { modelName = "gemma3"
          , prompt = "What is the meaning of life?"
          , stream = Just streamHandler
          }
  eRes <- generate ops Nothing
  case eRes of
    Left err -> putStrLn $ "Something went wrong: " ++ show err
    Right _ -> do
      putStrLn "LLM response completed"
