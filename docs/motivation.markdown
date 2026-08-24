---
title: Motivation & Architecture
category: Getting Started
description: Why Haskell is uniquely suited for robust, type-safe local AI pipelines.
---

## Why Haskell for Local LLMs?

Building production applications on top of Large Language Models introduces unique challenges:

1. **Unpredictable Output Formats**: Without strict type contracts, JSON responses from LLMs can suffer from missing fields, schema drift, or subtle type mismatches.
2. **Resource & Connection Exhaustion**: Naive HTTP streaming can lead to socket leaks, memory buffering bottlenecks, and unhandled connection dropouts.
3. **Complex State Management**: Multi-turn chat sessions and agentic tool-calling loops require thread-safe, concurrency-friendly state stores.

`ollama-haskell` addresses each of these challenges by leveraging Haskell's strong type system, high-performance runtime, and STM concurrency.

---

## Architectural Principles

```
┌─────────────────────────────────────────────────────────────┐
│                    User Application Code                    │
└──────────────────────────────┬──────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────┐
│                       Top-Level API                         │
│   (chat, generate, embed, listModels, pullStream, etc.)     │
└──────────────┬───────────────────────────────┬──────────────┘
               │                               │
               ▼                               ▼
┌─────────────────────────────┐ ┌─────────────────────────────┐
│      OllamaClient Core      │ │     Conduit Streaming       │
│  - Connection pooling (HTTP)│ │  - Constant memory bounds   │
│  - Retry policies & backoff │ │  - Zero-copy chunking       │
│  - Dynamic auth & headers   │ │  - Immediate backpressure   │
└──────────────┬──────────────┘ └──────────────┬──────────────┘
               │                               │
               ▼                               ▼
┌─────────────────────────────────────────────────────────────┐
│                   Ollama Local HTTP Daemon                  │
│                     (http://127.0.0.1:11434)                 │
└─────────────────────────────────────────────────────────────┘
```

---

## 1. Type-Safe Smart Constructors & Newtypes

Rather than passing raw string primitives, `ollama-haskell` provides dedicated newtypes with validation:

- **`ModelName`**: Validated non-empty model identifier.
- **`Think` / `ThinkingLevel`**: Explicit ADT for reasoning token budgets (`ThinkDisabled`, `ThinkLevel ThinkHigh`).
- **`Duration`**: Strongly-typed nanosecond duration parser with convenience helpers (`durationToSeconds`, `durationToMillis`).
- **`Digest`**: SHA-256 model digest validation.

---

## 2. Generic JSON Schema Derivation (`ToSchema`)

With `Ollama.Types.Format.SchemaDerive`, you never have to manually write error-prone JSON schema specifications:

```haskell
data UserProfile = UserProfile
  { username :: Text
  , age      :: Int
  , email    :: Maybe Text
  , tags     :: [Text]
  } deriving stock (Generic, Show)
    deriving anyclass (ToSchema, FromJSON)

-- Directly pass schema to LLM:
let req = (chatRequest "qwen3.5:2b" msgs) { chatFormat = Just (formatFor @UserProfile) }
```

GHC's compiler automatically derives the exact JSON schema, omits optional `Maybe` fields from the required array, and ensures the LLM's structured output maps directly to your Haskell data type.

---

## 3. High-Throughput Streaming with Conduit

Response streaming is powered by `conduit`. Unlike lazy I/O or callback systems, `conduit` guarantees:
- **Constant Memory Footprint**: Responses are processed token-by-token as they arrive from the socket.
- **Composable Transformations**: Easily pipe tokens to stdout, filter chunks, or fold them into accumulators using `collectStream` and `foldStream`.
- **Deterministic Resource Management**: Connection managers are automatically cleaned up even if downstream consumers encounter errors.

---

## 4. Concurrency & STM Conversation Storage

Managing conversation history in multi-threaded web servers is simplified with `Ollama.Conversation`:

```haskell
-- Thread-safe STM in-memory store
store <- initInMemoryStore
now   <- getCurrentTime
let conv = Conversation "session-42" [userMessage "Hello!"] "qwen3.5:9b" now now
saveConversationInMemory store conv
```

The transactional nature of STM ensures that concurrent requests to the same session will never result in corrupted history or race conditions.
