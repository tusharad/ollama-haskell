module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text.IO as TIO
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  putStrLn "=== Basic Chat with System Message and Metrics ==="
  let sysMsg = systemMessage "You are a concise, helpful assistant."
      usrMsg = userMessage "Why is the sky blue in one short sentence?"
      opts = Just (defaultOptions {optNumPredict = Just 30})
      req =
        (chatRequest "llama3.2:latest" (sysMsg :| [usrMsg]))
          { chatOptions = opts
          }

  res <- chat client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> case crMessage resp of
      Just msg -> do
        TIO.putStrLn $ "Response: " <> messageFullText msg
        case chatEvalTokensPerSecond resp of
          Just tps -> putStrLn $ "Generation speed: " <> show tps <> " tokens/sec"
          Nothing -> pure ()
        case chatPromptEvalTokensPerSecond resp of
          Just ptps -> putStrLn $ "Prompt evaluation speed: " <> show ptps <> " tokens/sec"
          Nothing -> pure ()
      Nothing -> putStrLn "No content returned"
