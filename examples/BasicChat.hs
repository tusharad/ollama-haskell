module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text.IO qualified as TIO
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  let opts = Just (defaultOptions {optNumPredict = Just 20})
      req =
        (chatRequest "qwen3.5:2b" (userMessage "Why is the sky blue?" :| []))
          { chatOptions = opts
          , chatThink = Just ThinkDisabled
          }
  res <- chat client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> case crMessage resp of
      Just msg -> TIO.putStrLn $ "Response: " <> messageContent msg
      Nothing -> putStrLn "No content returned"
