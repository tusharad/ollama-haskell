---
title: Testing & Mocking Infrastructure
category: Feature Tutorials
description: Write deterministic pure unit tests and CI pipelines without requiring a running Ollama server.
---

## Why Mock Testing for LLMs?

Running integration tests against a live LLM daemon during CI is often slow, non-deterministic, and requires heavy GPU compute.

`ollama-haskell` includes a built-in mock testing module (`Ollama.Testing`) that lets you test your business logic against mocked LLM responses with zero network overhead.

---

## 1. Creating a Mock Client

Use `newMockClient` or `withMockClient`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BSL
import Data.List.NonEmpty (NonEmpty ((:|)))
import Ollama
import Ollama.Testing
import Test.Tasty
import Test.Tasty.HUnit

-- Sample business logic that interacts with Ollama
askAssistant :: OllamaClient -> IO (Either OllamaError String)
askAssistant client = do
  let req = chatRequest "qwen3.5:2b" (userMessage "Ping" :| [])
  res <- chat client req
  pure $ fmap (maybe "" (show . messageContent) . crMessage) res

testMockChat :: TestTree
testMockChat = testCase "askAssistant returns mocked response" $ do
  -- 1. Create mock client with predefined serialized response
  let mockResp = mockChatResponse "qwen3.5:2b" "Pong from mock!"
  client <- newMockClient (BSL.toStrict (encode mockResp))

  -- 2. Execute business logic
  result <- askAssistant client

  -- 3. Assert deterministic outcome
  case result of
    Right content -> content @?= "\"Pong from mock!\""
    Left err      -> assertFailure $ "Unexpected error: " <> show err

main :: IO ()
main = defaultMain testMockChat
```

---

## 2. Available Mock Helpers

| Helper Function | Target Endpoint | Description |
| :--- | :--- | :--- |
| `mockChatResponse` | `chat` | Mocks chat completion responses with custom `Message` |
| `mockGenerateResponse` | `generate` | Mocks text generation with custom string response |
| `mockEmbedResponse` | `embed` | Mocks embedding vectors |
| `mockListModelsResponse` | `listModels` | Mocks list of installed local models |

All mock clients are fully thread-safe and can be used directly in test suites across multiple threads.
