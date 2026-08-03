module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text.IO qualified as TIO
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  let opts = Just (defaultOptions {optNumPredict = Just 20})
      req =
        (chatRequest "qwen3.5:2b" (userMessage "Count from 1 to 5." :| []))
          { chatOptions = opts
          , chatThink = Just ThinkDisabled
          }
  chunks <- collectStream (chatStream client req)
  mapM_ (TIO.putStr . maybe "" messageContent . crMessage) chunks
  putStrLn ""
