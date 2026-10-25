module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  putStrLn "=== Ollama Token Logprobs API ==="
  putStrLn "Sending chat request with chatLogprobs = Just True..."

  let opts = Just (defaultOptions {optNumPredict = Just 10})
      req =
        (chatRequest "llama3.2:latest" (userMessage "The capital of France is" :| []))
          { chatLogprobs = Just True
          , chatOptions = opts
          }

  res <- chat client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> do
      case crMessage resp of
        Just msg -> putStrLn $ "Response: " <> T.unpack (messageFullText msg)
        Nothing -> putStrLn "No message returned."

      case crLogprobs resp of
        Nothing -> putStrLn "No logprobs returned by model."
        Just [] -> putStrLn "Logprobs list is empty."
        Just tokens -> do
          putStrLn $ "\nReceived logprobs for " <> show (length tokens) <> " tokens:"
          mapM_
            ( \t -> do
                let tok = lpToken t
                    lp = lpLogprob t
                    prob = exp lp * 100.0 :: Double
                putStrLn $ "  Token: " <> show tok <> " | logprob: " <> show lp <> " | prob: " <> show prob <> "%"
                case lpTopLogprobs t of
                  Just tops | not (null tops) -> do
                    putStrLn "    Top alternative candidates:"
                    mapM_
                      ( \cand ->
                          putStrLn $
                            "      - "
                              <> show (tlpToken cand)
                              <> " (logprob: "
                              <> show (tlpLogprob cand)
                              <> ")"
                      )
                      (take 3 tops)
                  _ -> pure ()
            )
            (take 5 tokens)
