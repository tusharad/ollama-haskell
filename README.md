# ollama-haskell

[![Hackage](https://img.shields.io/hackage/v/ollama-haskell.svg)](https://hackage.haskell.org/package/ollama-haskell)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Modern Haskell client library for the [Ollama](https://ollama.com) local LLM engine.

## Features

- **Client-Centric Architecture**: Thread-safe `OllamaClient` handle with connection pooling and resource management (`newClient`, `defaultClient`, `clientFromEnv`, `withClient`).
- **First-Class Streaming**: `conduit`-based response streaming (`chatStream`, `generateStream`, `pullStream`, `pushStream`, `createModelStream`).
- **Model Context Protocol (MCP) Bridge**: Bidirectional integration with `mcp-server` for converting between Ollama tools and MCP tools, running MCP servers via stdio or HTTP (`Ollama.MCP`).
- **Generic JSON Schema Derivation**: Automatically derive JSON schemas from Haskell data types via `GHC.Generics` with `ToSchema` and `formatFor`.
- **Complete API Surface**: Text generation, chat completions, vector embeddings, model management (list, show, copy, delete, pull, push, create), and system endpoints.
- **Structured Outputs DSL**: Powerful `SchemaBuilder` DSL (`|+`, `|++`, `|!`, `|!!`) for type-safe JSON Schema structured responses.
- **Function / Tool Calling**: Full support for tool definitions (`Tool`), tool calls (`ToolCall`), and execution results (`toolResultMessage`).
- **Thinking Models Support**: Native support for reasoning models (`qwen3.5`, `deepseek-r1`) with `Think` / `ThinkingLevel` types.
- **Environment & Auth Integration**: Robust URL normalization for `OLLAMA_HOST` and bearer token support for `OLLAMA_API_KEY`.
- **Configurable Resilience**: Flexible retry policies (`NoRetry`, `ConstantRetry`, `ExponentialRetry`), custom timeouts, lifecycle callbacks, and structured logging.
- **Conversation Store**: Transactional STM-backed `InMemoryStore` and `ConversationStore` typeclass for managing multi-turn chat sessions.
- **SDK Comparison Matrix**: Detailed feature comparison against Python, JS/TS, and Go SDKs in [COMPARISON.md](docs/comparison.markdown).

---

## Installation

Add `ollama-haskell` to your `.cabal` file:

```cabal
build-depends:
    base >= 4.17 && < 5
  , ollama-haskell >= 0.4.1.0
```

Or using Stack in `package.yaml`:

```yaml
dependencies:
  - ollama-haskell >= 0.4.1.0
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

---

## Streaming Responses with Conduit

Stream LLM responses token-by-token in real time:

```haskell
import Conduit (mapM_C, runConduit, (.|))
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text.IO qualified as TIO
import Ollama
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  client <- defaultClient
  let req = chatRequest "qwen3.5:2b" (userMessage "Count from 1 to 5." :| [])

  -- Stream tokens to stdout as they arrive
  runConduit $
    chatStream client req .| mapM_C (\chunk -> liftIO $ do
      mapM_ (TIO.putStr . messageContent) (crMessage chunk)
      hFlush stdout
    )
  putStrLn ""
```

You can also accumulate all chunks at once with `collectStream`, or fold text with `foldStream`:

```haskell
-- Collect all chunks:
chunks <- collectStream (chatStream client req)

-- Or fold into a single Text value:
fullText <- foldStream (\acc c -> acc <> maybe "" messageContent (crMessage c)) "" (chatStream client req)
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

Enforce structured JSON output formats using `SchemaBuilder` (re-exported directly from `Ollama`):

```haskell
import Data.Text.IO qualified as TIO
import Ollama

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
  , configRetry   = ExponentialRetry 3 1000000 -- 3 retries with exponential backoff
  , configLogger  = Just (\level msg -> putStrLn $ "[" <> show level <> "] " <> show msg)
  }

main :: IO ()
main = withClient customConfig $ \client -> do
  ...
```

---

## Feature Matrix

| Feature | Haskell (`ollama-haskell`) | Official Python (`ollama-python`) | Official JS/TS (`ollama-js`) | Community Go (`ollama/ollama`) |
| :--- | :---: | :---: | :---: | :---: |
| **Strict Type Safety** | ✅ Compile-time (PVP, Smart Constructors) | ⚠️ Type hints (Runtime) | ⚠️ TypeScript (Erased at runtime) | ✅ Go Structs |
| **Response Streaming** | ✅ `conduit` ($O(1)$ constant memory) | ⚠️ Python Generator | ⚠️ Async Iterator | ⚠️ Go Channels |
| **Structured Output Derivation** | ✅ `GHC.Generics` (`ToSchema`) | ⚠️ Pydantic BaseModel | ⚠️ Zod / JSON Schema | ⚠️ Manual JSON Schema |
| **Model Context Protocol (MCP)** | ✅ Native `mcp-server` Bridge | ❌ Manual | ❌ Manual | ❌ Manual |
| **Thinking / Reasoning Models** | ✅ Dedicated `Think` ADT | ⚠️ Dict parameters | ⚠️ Object properties | ⚠️ Raw parameters |
| **Transactional Chat Store** | ✅ STM `InMemoryStore` | ❌ None | ❌ None | ❌ None |
| **Built-in Mock Testing** | ✅ `Ollama.Testing` (Pure) | ❌ None | ❌ None | ❌ None |
| **Configurable Retry & Backoff** | ✅ Exponential & Constant ADT | ❌ Manual | ❌ Manual | ❌ Manual |
| **Token Throughput Metrics** | ✅ Native Calculation Helpers | ⚠️ Raw nanoseconds | ⚠️ Raw nanoseconds | ⚠️ Raw nanoseconds |
| **Environment Auto-Discovery** | ✅ `clientFromEnv` | ✅ Default client | ✅ Default client | ✅ Default client |

---

## Documentation & References

- [Comparison Deep Dive](docs/comparison.markdown) — Detailed architectural comparison across language ecosystems.
- [CONTRIBUTING.md](CONTRIBUTING.md) — Development setup, testing guidelines, and code style.
- [CHANGELOG.md](CHANGELOG.md) — Release notes and version changelog.
- [Hackage Documentation](https://hackage.haskell.org/package/ollama-haskell) — Full Haddock API reference.

---

## License

MIT © 2024–2026 Tushar Adhatrao

