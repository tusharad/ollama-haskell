{-# LANGUAGE OverloadedStrings #-}

module Example.GenerateWithModelOptions (runApp) where

import Data.Ollama.Generate
import qualified Data.Text.IO as T

runApp :: IO ()
runApp = do
  let modelOps =
        defaultModelOptions
          { numCtx = Just 10000
          , temperature = Just 0.8
          }
  -- \^ By default Ollama have a small context window

  let ops =
        defaultGenerateOps
          { modelName = "gemma3"
          , prompt = "What is the meaning of life?"
          , options = Just modelOps
          }
  eRes <- generate ops Nothing
  case eRes of
    Left err -> putStrLn $ "Something went wrong: " ++ show err
    Right r -> do
      putStrLn "LLM response"
      T.putStrLn (genResponse r)
