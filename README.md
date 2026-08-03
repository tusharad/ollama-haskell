# ollama-haskell

[![Hackage](https://img.shields.io/hackage/v/ollama-haskell.svg)](https://hackage.haskell.org/package/ollama-haskell)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Industry-grade, feature-complete, modern Haskell client library for the [Ollama](https://ollama.com) local LLM engine.

## Features

- **Client-Centric Architecture**: Thread-safe `OllamaClient` handle with connection pooling and resource management (`newClient`, `defaultClient`, `clientFromEnv`, `withClient`).
- **First-Class Streaming**: `conduit`-based response streaming (`chatStream`, `generateStream`, `pullStream`, `pushStream`, `createModelStream`).
- **Complete API Surface**: Text generation, chat completions, vector embeddings, model management (list, show, copy, delete, pull, push, create), and system endpoints.
- **Structured Outputs**: Powerful `SchemaBuilder` DSL (`|+`, `|++`, `|!`, `|!!`) for type-safe JSON Schema structured responses.
- **Function / Tool Calling**: Full support for tool definitions (`Tool`), tool calls (`ToolCall`), and execution results (`toolResultMessage`).
- **Thinking Models Support**: Native support for reasoning models (`qwen3.5`, `deepseek-r1`) with `Think` / `ThinkingLevel` types.
- **Environment & Auth Integration**: Robust URL normalization for `OLLAMA_HOST` and bearer token support for `OLLAMA_API_KEY`.
- **Configurable Resilience**: Flexible retry policies (`NoRetry`, `ConstantRetry`, `ExponentialRetry`), lifecycle callbacks, and structured logging.
- **Conversation Store**: Transactional STM-backed `InMemoryStore` and `ConversationStore` typeclass for managing multi-turn chat sessions.
- **SDK Comparison Matrix**: Detailed feature comparison against Python, JS/TS, and Go SDKs in [doc/COMPARISON.md](doc/COMPARISON.md).

---

## Installation

Add `ollama-haskell` to your `.cabal` file:

```cabal
build-depends:
    base >= 4.17 && < 5
  , ollama-haskell >= 0.3.0.0
```

Or using Stack in `package.yaml`:

```yaml
dependencies:
  - ollama-haskell >= 0.3.0.0
```

---

## Quick Start (5 Lines)

```haskell
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text.IO qualified as TIO
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  res <- chat client $ chatRequest "qwen3.5:2b" (userMessage "Why is the sky blue?" :| [])
  case res of
    Left err   -> print err
    Right resp -> mapM_ (TIO.putStrLn . messageContent) (crMessage resp)
```

---

## Streaming Responses with Conduit

Stream LLM responses token-by-token as they generate:

```haskell
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text.IO qualified as TIO
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  let req = chatRequest "qwen3.5:2b" (userMessage "Count from 1 to 5." :| [])
  
  -- Stream chunks directly into stdout or collect them
  chunks <- collectStream (chatStream client req)
  mapM_ (TIO.putStr . maybe "" messageContent . crMessage) chunks
  putStrLn ""
```

---

## Function & Tool Calling

Define function signatures and let the LLM execute structured tool calls:

```haskell
import Data.List.NonEmpty (NonEmpty ((:|)))
import Ollama

calculatorTool :: Tool
calculatorTool = Tool "function" $ FunctionDef
  { fnName = "add"
  , fnDescription = Just "Add two numbers"
  , fnParameters = Just (FunctionParameters "object" Nothing (Just ["a", "b"]) Nothing Nothing Nothing)
  , fnStrict = Just True
  }

main :: IO ()
main = do
  client <- defaultClient
  let req = (chatRequest "qwen3.5:2b" (userMessage "What is 40 + 2?" :| []))
        { chatTools = Just [calculatorTool] }
  res <- chat client req
  case res of
    Left err   -> print err
    Right resp -> print (crMessage resp)
```

---

## Structured Outputs (JSON Schema DSL)

Enforce structured JSON output formats using `SchemaBuilder`:

```haskell
import Data.Text.IO qualified as TIO
import Ollama
import Ollama.Types.Format.SchemaBuilder

personSchema :: Schema
personSchema = buildSchema $ emptyObject
  |+ ("name", JString)
  |+ ("age", JInteger)
  |! "name"

main :: IO ()
main = do
  client <- defaultClient
  let req = (generateRequest "qwen3.5:2b" "Generate a person profile.")
        { genFormat = Just (SchemaFormat personSchema) }
  res <- generate client req
  case res of
    Left err   -> print err
    Right resp -> TIO.putStrLn (grResponse resp)
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
  , configRetry   = ExponentialRetry 3 1 -- 3 retries with exponential backoff
  , configLogger  = Just (\level msg -> putStrLn $ "[" <> show level <> "] " <> show msg)
  }

main :: IO ()
main = withClient customConfig $ \client -> do
  ...
```

---

## Documentation & SDK Comparison

- [doc/COMPARISON.md](doc/COMPARISON.md) — SDK Feature Matrix comparing `ollama-haskell` with Python, JS/TS, and Go SDKs.
- [ARCHITECTURE.md](ARCHITECTURE.md) — Detailed internal module design and extension guide.
- [CONTRIBUTING.md](CONTRIBUTING.md) — Development setup, testing guidelines, and code style.
- [CHANGELOG.md](CHANGELOG.md) — Release notes and changelog.
- [Hackage Documentation](https://hackage.haskell.org/package/ollama-haskell) — Full Haddock reference.

---

## License

MIT © 2024–2026 Tushar Adhatrao
