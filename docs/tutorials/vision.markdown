---
title: Multimodal & Vision Models
category: Feature Tutorials
description: Pass images to multimodal vision models (Gemma 3, Qwen 2.5/3.5-VL, LLaVA) using Base64Image and readImageBase64.
---

## Multimodal Support in ollama-haskell

Vision models can analyze image content alongside textual prompts. `ollama-haskell` provides built-in utilities for loading, encoding, and attaching image data to both `chat` messages and `generate` completions.

---

## 1. Loading & Encoding Images

Use `readImageBase64` to load an image from disk without third-party boilerplate:

```haskell
import Ollama

-- Reads raw image bytes and encodes as Base64Image
img <- readImageBase64 "chart.png"
```

Or convert raw in-memory `ByteString` using `encodeBase64Image`:

```haskell
let img = encodeBase64Image rawByteString
```

---

## 2. Multimodal Chat

Construct an image message using `imageMessage :: Text -> [Base64Image] -> Message`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text.IO as TIO
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  img <- readImageBase64 "diagram.png"

  let msg = imageMessage "Describe the architecture shown in this diagram." [img]
      req = chatRequest "gemma3:latest" (msg :| [])

  res <- chat client req
  case res of
    Left err -> print err
    Right resp -> case crMessage resp of
      Just reply -> TIO.putStrLn (messageFullText reply)
      Nothing -> putStrLn "No response returned"
```

---

## 3. Multimodal Generation

You can also pass images to the single-turn `generate` completion endpoint using `genImages`:

```haskell
let req = (generateRequest "gemma3:latest" "Extract all table values from this screenshot.")
      { genImages = Just [img] }

res <- generate client req
case res of
  Left err -> print err
  Right resp -> TIO.putStrLn (generateFullText resp)
```
