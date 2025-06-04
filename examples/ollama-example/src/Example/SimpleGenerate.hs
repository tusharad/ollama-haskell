{-# LANGUAGE OverloadedStrings #-}

module Example.SimpleGenerate (runApp) where

import Data.Ollama.Generate
import qualified Data.Text.IO as T

runApp :: IO ()
runApp = do
  let ops =
        defaultGenerateOps
          { modelName = "gemma3"
          , prompt = "What is the meaning of life?"
          }
  eRes <- generate ops Nothing
  case eRes of
    Left err -> putStrLn $ "Something went wrong: " ++ show err
    Right r -> do
      putStrLn "LLM response"
      T.putStrLn (genResponse r)
