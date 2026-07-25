# ollama-haskell

[![Hackage](https://img.shields.io/hackage/v/ollama-haskell.svg)](https://hackage.haskell.org/package/ollama-haskell)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

High-performance, feature-complete, modern Haskell client library for the [Ollama](https://ollama.com) local LLM engine.

## Features

- **Client-Centric Architecture**: Thread-safe `OllamaClient` handle with connection pooling and resource management.
- **First-Class Streaming**: `conduit`-based response streaming (`chatStream`, `generateStream`, `pullStream`, `pushStream`, `createModelStream`).
- **Complete API Surface**: Text generation, chat completions, vector embeddings, model management (list, show, copy, delete, pull, push, create), and system endpoints.
- **Structured Outputs**: Powerful `SchemaBuilder` DSL for JSON Schema structured responses.
- **Function / Tool Calling**: Full support for tool definitions (`Tool`), tool calls (`ToolCall`), and execution results (`toolResultMessage`).
- **Thinking Models Support**: native support for thinking models (`qwen3.5`, `deepseek-r1`) with `Think` / `ThinkingLevel` types.
- **Environment & Auth Integration**: Robust URL normalization for `OLLAMA_HOST` and bearer token support for `OLLAMA_API_KEY`.
- **Configurable Resilience**: Flexible retry policies (`NoRetry`, `ConstantRetry`, `ExponentialRetry`), lifecycle callbacks, and structured logging.
- **Conversation Store**: Transactional STM-backed `InMemoryStore` and `ConversationStore` typeclass for managing multi-turn chat sessions.

---

## Installation

Add `ollama-haskell` to your `.cabal` file:

```cabal
build-depends:
    base >= 4.17 && < 5
  , ollama-haskell >= 1.0.0.0
```

Or using Stack in `package.yaml`:

```yaml
dependencies:
  - ollama-haskell >= 1.0.0.0
```

---

## Quick Start (5 Lines)

```haskell
import Data.List.NonEmpty (NonEmpty ((:|)))
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  res <- chat client $ chatRequest "llama3.2" (userMessage "Why is the sky blue?" :| [])
  case res of
    Left err   -> print err
    Right resp -> mapM_ (putStrLn . messageContent) (crMessage resp)
```

---

## Streaming Responses with Conduit

Stream LLM responses token-by-token as they generate:

```haskell
import Data.List.NonEmpty (NonEmpty ((:|)))
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  let req = chatRequest "llama3.2" (userMessage "Count from 1 to 10." :| [])
  
  -- Stream chunks directly into stdout or collect them
  chunks <- collectStream (chatStream client req)
  mapM_ (putStrLn . maybe "" messageContent . crMessage) chunks
```

---

## Function & Tool Calling

Define function signatures and let the LLM execute structured tool calls:

```haskell
import Data.List.NonEmpty (NonEmpty ((:|)))
import Ollama

calculatorTool :: Tool
calculatorTool = Tool "function" $ FunctionDef
  { fnName = "add_numbers"
  , fnDescription = "Add two numbers together"
  , fnParameters = FunctionParameters "object" ...
  }

main :: IO ()
main = do
  client <- defaultClient
  let req = (chatRequest "llama3.2" (userMessage "What is 40 + 2?" :| []))
        { chatTools = Just [calculatorTool] }
  res <- chat client req
  case res of
    Left err -> print err
    Right resp -> print (crMessage resp)
```

---

## Structured Outputs (JSON Schema DSL)

Enforce structured JSON output formats using `SchemaBuilder`:

```haskell
import Ollama

personSchema :: Schema
personSchema = buildSchema $ emptyObject
  |+ ("name", JString)
  |+ ("age", JInteger)
  |! "name"

main :: IO ()
main = do
  client <- defaultClient
  let req = (generateRequest "llama3.2" "Generate a person profile.")
        { genFormat = Just (SchemaFormat personSchema) }
  res <- generate client req
  case res of
    Left err   -> print err
    Right resp -> putStrLn (grResponse resp)
```

---

## Environment Variables & Configuration

Construct a client using environment variables (`OLLAMA_HOST`, `OLLAMA_API_KEY`):

```haskell
main :: IO ()
main = do
  client <- clientFromEnv
  -- Automatically connects to OLLAMA_HOST with optional Authorization: Bearer header
  ...
```

Or configure custom retry policies and loggers:

```haskell
customConfig :: OllamaClientConfig
customConfig = defaultConfig
  { configBaseUrl = "http://my-ollama-server:11434"
  , configTimeout = 120
  , configRetry   = ExponentialRetry 3 1000000 -- 3 retries with backoff
  , configLogger  = Just (\level msg -> putStrLn $ "[" <> show level <> "] " <> show msg)
  }

main :: IO ()
main = withClient customConfig $ \client -> do
  ...
```

---

## Architecture & Documentation

- [ARCHITECTURE.md](ARCHITECTURE.md) — Detailed internal module design and extension guide.
- [CONTRIBUTING.md](CONTRIBUTING.md) — Development setup, testing guidelines, and code style.
- [CHANGELOG.md](CHANGELOG.md) — Release notes and migration guide from v0.2.x.
- [Hackage Documentation](https://hackage.haskell.org/package/ollama-haskell) — Full Haddock reference.

---

## License

MIT © 2024–2026 Tushar Adhatrao
