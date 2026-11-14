---
title: Model Lifecycle Management
category: Feature Tutorials
description: List, pull with progress streaming, copy, create, and delete local models programmatically.
---

## Overview

`ollama-haskell` provides full programmatic control over your locally downloaded models and their VRAM lifecycle.

---

## 1. List Downloaded Models & VRAM Usage

Query all models installed on the local system:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Ollama

main :: IO ()
main = do
  client <- defaultClient

  -- 1. List all available models on disk
  listRes <- listModels client
  case listRes of
    Right (ListResponse ms) -> do
      putStrLn "--- Local Models ---"
      mapM_ (\m -> putStrLn $ "• " <> show (miName m) <> " (" <> show (miSize m `div` 1000000) <> " MB)") ms
    Left err -> print err

  -- 2. Check models currently loaded in GPU VRAM
  psRes <- listRunning client
  case psRes of
    Right (RunningModelsResponse rms) -> do
      putStrLn "\n--- Active Models in VRAM ---"
      mapM_ (\rm -> putStrLn $ "• " <> show (rmName rm) <> " [VRAM: " <> show (rmSizeVram rm `div` 1000000) <> " MB]") rms
    Left err -> print err
```

---

## 2. Pulling Models with Progress Streaming

Download new models from Ollama library while printing real-time download percentages:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Conduit
import Ollama
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  client <- defaultClient

  putStrLn "Pulling 'qwen3.5:2b'..."
  runConduit $
    pullStream client "qwen3.5:2b"
    .| mapM_C (\chunk -> do
        case (prTotal chunk, prCompleted chunk) of
          (Just total, Just completed) -> do
            let pct = (completed * 100) `div` total
            liftIO $ do
              putStr $ "\rStatus: " <> show (prStatus chunk) <> " [" <> show pct <> "%]"
              hFlush stdout
          _ -> liftIO $ do
            putStr $ "\rStatus: " <> show (prStatus chunk)
            hFlush stdout
      )

  putStrLn "\nPull complete!"
```

---

## 3. Copying & Deleting Models

```haskell
-- Duplicate an existing model under a custom alias
_ <- copyModel client "qwen3.5:2b" "my-custom-assistant"

-- Delete a model from disk
_ <- deleteModel client "my-custom-assistant"
```
