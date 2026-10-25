module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (isJust)
import qualified Data.Text.IO as TIO
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  let model = "qwen3.5:2b"
      opts = Just (defaultOptions {optNumPredict = Just 80})

  putStrLn "=== 1. Chat with Thinking Mode Disabled (ThinkDisabled) ==="
  let reqNoThink =
        (chatRequest model (userMessage "Which is larger: 9.11 or 9.9?" :| []))
          { chatThink = Just ThinkDisabled
          , chatOptions = opts
          }
  resNoThink <- chat client reqNoThink
  case resNoThink of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> case crMessage resp of
      Just msg -> do
        putStrLn $ "Thinking present? " <> show (isJust (messageThinking msg))
        putStrLn "Response content:"
        TIO.putStrLn (messageContent msg)
      Nothing -> putStrLn "No message returned."

  putStrLn "\n=== 2. Chat with Thinking Mode Enabled (ThinkEnabled) ==="
  let reqThink =
        (chatRequest model (userMessage "Which is larger: 9.11 or 9.9?" :| []))
          { chatThink = Just ThinkEnabled
          , chatOptions = opts
          }
  resThink <- chat client reqThink
  case resThink of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> case crMessage resp of
      Just msg -> do
        case messageThinking msg of
          Just th -> do
            putStrLn "Extracted Thinking Trace:"
            TIO.putStrLn th
          Nothing -> putStrLn "No thinking content returned."
        putStrLn "\nFinal Content:"
        TIO.putStrLn (messageContent msg)
        putStrLn "\nFull Combined Text (via messageFullText):"
        TIO.putStrLn (messageFullText msg)
      Nothing -> putStrLn "No message returned."

  putStrLn "\n=== 3. Custom Thinking Level (ThinkCustom) ==="
  let customThink = ThinkCustom "budget_2048"
  putStrLn $ "Demonstrating custom thinking level: " <> show customThink
