---
title: Reasoning & Thinking Models
category: Feature Tutorials
description: Control and inspect chain-of-thought reasoning tokens with deep thinking models like DeepSeek-R1 and Qwen 3.5.
---

## What are Thinking Models?

Modern reasoning models (such as `deepseek-r1` and `qwen3.5`) output internal "thinking" or chain-of-thought tokens before providing their final answer.

`ollama-haskell` provides dedicated ADTs to control whether thinking is enabled, disabled, or tuned to specific reasoning depth levels.

---

## 1. The `Think` ADT

In `Ollama.Types.Message` and `Ollama.Types.Common`:

```haskell
data Think
  = ThinkEnabled                  -- Enable default model thinking
  | ThinkDisabled                 -- Suppress reasoning tokens for faster response
  | ThinkLevel !ThinkingLevel     -- Specify standard reasoning effort level
  | ThinkCustom !Text             -- Pass custom model-specific thinking level/budget

data ThinkingLevel
  = ThinkLow
  | ThinkMedium
  | ThinkHigh
  | ThinkMax
```

---

## 2. Using Thinking Levels in Queries

Configure `chatThink` or `genThink` in your requests:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text.IO as TIO
import Ollama

main :: IO ()
main = do
  client <- defaultClient

  let puzzlePrompt = userMessage "How many 'r's are in the word strawberry? Think step-by-step." :| []

      -- Enable deep reasoning effort
      req = (chatRequest "deepseek-r1:8b" puzzlePrompt)
        { chatThink = Just (ThinkLevel ThinkHigh) }

  res <- chat client req
  case res of
    Left err   -> print err
    Right resp -> case crMessage resp of
      Just msg -> do
        -- Inspect thoughts separately
        case messageThinking msg of
          Just thoughts -> do
            putStrLn "--- Chain-of-Thought Reasoning ---"
            TIO.putStrLn thoughts
          Nothing -> pure ()

        putStrLn "\n--- Final Content ---"
        TIO.putStrLn (messageContent msg)

        -- Or use messageFullText to retrieve both thinking and final content safely
        putStrLn "\n--- Complete Output (via messageFullText) ---"
        TIO.putStrLn (messageFullText msg)
      Nothing -> putStrLn "No message returned"
```

---

## 3. Disabling Thinking for Ultra-Low Latency

When building autocomplete or simple conversational agents where latency is critical, disable thinking explicitly:

```haskell
let fastReq = req { chatThink = Just ThinkDisabled }
```

