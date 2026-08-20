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

Use the `Tool` and `FunctionDef` types to declare your tool signature:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (Value (..), decode)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Ollama

-- 1. Define the Calculator Tool
calculatorTool :: Tool
calculatorTool = Tool "function" $ FunctionDef
  { fnName = "get_stock_price"
  , fnDescription = Just "Fetch the current stock price for a given ticker symbol."
  , fnParameters = Just FunctionParameters
      { fpType = "object"
      , fpProperties = Just $ Map.fromList
          [ ( "symbol"
            , FunctionParameters
                { fpType = "string"
                , fpProperties = Nothing
                , fpRequired = Nothing
                , fpDescription = Just "Stock ticker symbol (e.g. AAPL, GOOGL)"
                , fpEnum = Nothing
                }
            )
          ]
      , fpRequired = Just ["symbol"]
      , fpDescription = Nothing
      , fpEnum = Nothing
      }
  , fnStrict = Just True
  }
```

---

## 2. Multi-Turn Tool Execution Loop

Pass the tool in `chatTools` and inspect the model's response:

```haskell
main :: IO ()
main = do
  client <- defaultClient

  let userMsg = userMessage "What is the stock price of AAPL?"
      req = (chatRequest "qwen3.5:2b" (userMsg :| []))
        { chatTools = Just [calculatorTool] }

  putStrLn "Sending user prompt to model..."
  res <- chat client req

  case res of
    Left err -> print err
    Right resp -> case crMessage resp of
      Just assistantMsg -> do
        case messageToolCalls assistantMsg of
          Just (toolCall : _) -> do
            let fn = tcFunction toolCall
            putStrLn $ "LLM requested tool call: " <> show (tcfName fn)
            putStrLn $ "With arguments: " <> show (tcfArguments fn)

            -- 3. Execute custom Haskell logic
            let resultText = "AAPL is currently trading at $234.50 USD (+1.8%)"

            -- 4. Send tool result back to the model
            let toolRespMsg = toolResultMessage resultText
                followUpHistory = userMsg :| [assistantMsg, toolRespMsg]
                followUpReq = chatRequest "qwen3.5:2b" followUpHistory

            finalRes <- chat client followUpReq
            case finalRes of
              Right finalResp -> mapM_ (print . messageContent) (crMessage finalResp)
              Left err        -> print err

          Nothing ->
            putStrLn $ "Model responded directly: " <> show (messageContent assistantMsg)
      Nothing -> putStrLn "No message returned"
```
