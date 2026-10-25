---
title: Function & Tool Calling
category: Feature Tutorials
description: Give local LLMs access to custom Haskell functions and external APIs with structured tool calling.
---

## What is Tool Calling?

Tool calling enables the language model to invoke your custom Haskell functions when it needs external data (such as querying a database, calculating math, or checking the current weather).

The model does not run the code itself; instead, it returns a structured `ToolCall` containing the function name and arguments. Your Haskell program executes the function and passes the result back to the model.

---

## 1. Define a Tool in Haskell

Use the ergonomic smart constructors `toolDef`, `objectParams`, and `stringParam` (or `numberParam`, `intParam`, `boolParam`, `enumParam`):

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Ollama

-- 1. Define the Stock Price Tool using smart constructors
stockTool :: Tool
stockTool =
  toolDef "get_stock_price" "Fetch the current stock price for a given ticker symbol." $
    objectParams
      [ ("symbol", stringParam (Just "Stock ticker symbol (e.g. AAPL, GOOGL)")) ]
      ["symbol"]
```

---

## 2. Multi-Turn Tool Execution Loop

Pass the tool in `chatTools` and inspect the model's response:

```haskell
main :: IO ()
main = do
  client <- defaultClient

  let userMsg = userMessage "What is the stock price of AAPL?"
      req = (chatRequest "llama3.2:latest" (userMsg :| []))
        { chatTools = Just [stockTool] }

  putStrLn "Sending user prompt to model..."
  res <- chat client req

  case res of
    Left err -> print err
    Right resp -> case crMessage resp of
      Just assistantMsg -> do
        case messageToolCalls assistantMsg of
          Just (toolCall : _) -> do
            let fn = tcFunction toolCall
                callId = tcId toolCall
            putStrLn $ "LLM requested tool call: " <> show (tcfName fn)
            putStrLn $ "Call ID: " <> show callId
            putStrLn $ "With arguments: " <> show (tcfArguments fn)

            -- 3. Execute custom Haskell logic
            let resultText = "AAPL is currently trading at $234.50 USD (+1.8%)"

            -- 4. Send tool result back to the model (with optional call ID)
            let toolRespMsg = case callId of
                  Just cid -> toolResultMessageWithId resultText (tcfName fn) cid
                  Nothing  -> toolResultMessage resultText (tcfName fn)
                followUpHistory = userMsg :| [assistantMsg, toolRespMsg]
                followUpReq = (chatRequest "llama3.2:latest" followUpHistory)
                  { chatTools = Just [stockTool] }

            finalRes <- chat client followUpReq
            case finalRes of
              Right finalResp -> mapM_ (print . messageFullText) (crMessage finalResp)
              Left err        -> print err

          _ ->
            putStrLn $ "Model responded directly: " <> show (messageFullText assistantMsg)
      Nothing -> putStrLn "No message returned"
```
