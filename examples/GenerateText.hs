module Main (main) where

import qualified Data.Text.IO as TIO
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  putStrLn "=== 1. Non-Streaming Text Generation ==="
  let req =
        (generateRequest "llama3.2:latest" "Write a haiku about Haskell compilers.")
          { genOptions = Just (defaultOptions {optTemperature = Just 0.7, optNumPredict = Just 40})
          }
  res <- generate client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> do
      putStrLn "Generated Response:"
      TIO.putStrLn (generateFullText resp)
      case evalTokensPerSecond resp of
        Just tps -> putStrLn $ "Evaluation speed: " <> show tps <> " tokens/sec"
        Nothing -> pure ()
      case promptEvalTokensPerSecond resp of
        Just ptps -> putStrLn $ "Prompt evaluation speed: " <> show ptps <> " tokens/sec"
        Nothing -> pure ()

  putStrLn "\n=== 2. Streaming Text Generation with Conduit ==="
  let streamReq =
        (generateRequest "llama3.2:latest" "Name three benefits of pure functional programming.")
          { genOptions = Just (defaultOptions {optNumPredict = Just 50})
          }
  putStrLn "Streaming response chunks:"
  chunks <- collectStream (generateStream client streamReq)
  mapM_ (TIO.putStr . grResponse) chunks
  putStrLn "\n--- Stream completed ---"
