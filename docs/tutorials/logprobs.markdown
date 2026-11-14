---
title: Token Log Probabilities (Logprobs)
category: Feature Tutorials
description: Inspect per-token confidence and alternative candidate log probabilities returned by Ollama.
---

## What are Logprobs?

Log probabilities (`logprobs`) measure the model's confidence for each emitted token. A logprob of `0.0` corresponds to 100% certainty ($e^0 = 1$), while increasingly negative values correspond to lower probability ($e^{-1} \approx 36.8\%$, $e^{-2} \approx 13.5\%$).

---

## Requesting Logprobs

Set `chatLogprobs = Just True` on `ChatRequest` (or `genLogprobs = Just True` on `GenerateRequest`):

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import Ollama

main :: IO ()
main = do
  client <- defaultClient

  let req = (chatRequest "llama3.2:latest" (userMessage "The capital of France is" :| []))
        { chatLogprobs = Just True }

  res <- chat client req
  case res of
    Left err -> print err
    Right resp -> case crLogprobs resp of
      Nothing -> putStrLn "No logprobs returned"
      Just tokens -> do
        putStrLn "Token Probabilities:"
        mapM_ (\t -> do
          let tok = lpToken t
              lp = lpLogprob t
              prob = exp lp * 100.0 :: Double
          putStrLn $ "  - " <> show tok <> ": " <> show prob <> "% (logprob: " <> show lp <> ")"
          case lpTopLogprobs t of
            Just tops | not (null tops) -> do
              putStrLn "    Alternatives:"
              mapM_ (\alt -> putStrLn $ "      * " <> show (tlpToken alt) <> ": " <> show (tlpLogprob alt)) (take 3 tops)
            _ -> pure ()
          ) tokens
```
