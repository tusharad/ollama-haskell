# ollama-haskell

[![Hackage](https://img.shields.io/hackage/v/ollama-haskell.svg)](https://hackage.haskell.org/package/ollama-haskell)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Modern Haskell client library for the [Ollama](https://ollama.com) local LLM engine.

## Features

- **Client-Centric Architecture**: Thread-safe `OllamaClient` handle with connection pooling and resource management (`newClient`, `defaultClient`, `clientFromEnv`, `withClient`).
- **First-Class Streaming**: `conduit`-based response streaming (`chatStream`, `generateStream`, `pullStream`, `pushStream`, `createModelStream`).
- **Model Context Protocol (MCP) Bridge**: Bidirectional integration with `mcp-server` for converting between Ollama tools and MCP tools, running MCP servers via stdio or HTTP (`Ollama.MCP`).
- **Generic JSON Schema Derivation**: Automatically derive JSON schemas from Haskell data types via `GHC.Generics` with `ToSchema` and `formatFor`.
- **Complete API Surface**: Text generation, chat completions, vector embeddings, model management (list, show, copy, delete, pull, push, create), system endpoints, and System One decision evaluation.
- **System One Decision Models**: Native support for `/v1/systemone` fast classification, binary probability (`noul`), and scoring (`choiceQuestion`, `noulQuestion`, `scoreQuestion`).
- **Structured Outputs DSL**: Powerful `SchemaBuilder` DSL (`|+`, `|++`, `|!`, `|!!`) for type-safe JSON Schema structured responses.
- **Function / Tool Calling**: Full support for tool definitions (`Tool`), tool calls (`ToolCall`), and execution results (`toolResultMessage`).
- **Thinking Models & Logprobs**: Native support for reasoning models (`Think` / `ThinkingLevel` with custom model-defined levels) and token log probabilities (`Logprob` / `TokenLogprob`).
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
  , ollama-haskell >= 0.4.2.0
```

Or using Stack in `package.yaml`:

```yaml
dependencies:
  - ollama-haskell >= 0.4.2.0
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

Define function signatures with zero boilerplate using `toolDef`, `objectParams`, and typed parameters:

```haskell
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Ollama

calculatorTool :: Tool
calculatorTool =
  toolDef "add" "Add two numbers" $
    objectParams
      [ ("a", numberParam (Just "First number"))
      , ("b", numberParam (Just "Second number"))
      ]
      ["a", "b"]

main :: IO ()
main = do
  client <- defaultClient
  let req = (chatRequest "llama3.2:latest" (userMessage "What is 25 + 17?" :| []))
        { chatTools = Just [calculatorTool] }
  res <- chat client req
  case res of
    Left err   -> print err
    Right resp -> case crMessage resp of
      Just msg -> case messageToolCalls msg of
        Just (call : _) -> do
          putStrLn $ "Tool called: " <> show (tcfName (tcFunction call))
          putStrLn $ "Arguments: " <> show (tcfArguments (tcFunction call))
        _ -> putStrLn "No tool called."
      Nothing -> putStrLn "No message returned."
```

---

## Multimodal Vision

Load local images directly and send them to vision models (`gemma3`, `qwen3.5:2b`, etc.):

```haskell
main :: IO ()
main = do
  client <- defaultClient
  img <- readImageBase64 "chart.png"
  let msg = imageMessage "Describe the chart in 2 sentences." [img]
  res <- chat client (chatRequest "gemma3:latest" (msg :| []))
  case res of
    Right resp -> mapM_ (putStrLn . messageFullText) (crMessage resp)
    Left err   -> print err
```

---

## Thinking / Reasoning Models

Control chain-of-thought tokens on reasoning models (e.g. DeepSeek-R1, Qwen 3.5):

```haskell
let req = (chatRequest "qwen3.5:2b" (userMessage "Which is larger: 9.11 or 9.9?" :| []))
      { chatThink = Just ThinkEnabled } -- or ThinkDisabled, ThinkLevel ThinkHigh, ThinkCustom "..."
res <- chat client req
case res of
  Right resp -> case crMessage resp of
    Just msg -> do
      case messageThinking msg of
        Just th -> putStrLn $ "Thinking trace:\n" <> th
        Nothing -> pure ()
      putStrLn $ "Answer:\n" <> messageContent msg
    Nothing -> pure ()
  Left err -> print err
```

---

## System One Decision Models

Evaluate high-speed classification, probability (`noul`), and scoring with `/v1/systemone`:

```haskell
let q1 = choiceQuestion "Customer eligibility" (Map.fromList [("eligible", Just "Standard"), ("ineligible", Nothing)])
    q2 = noulQuestion "Will this cause churn?"
    req = systemOneRequest "nimble" "Customer account history..." (Map.fromList [("q1", q1), ("q2", q2)])
res <- systemOne client req
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

## Runnable Examples

The repository includes complete, tested runnable examples for every library feature:

| Target | Source File | Description |
| :--- | :--- | :--- |
| `ollama-example-basic-chat` | [`examples/BasicChat.hs`](examples/BasicChat.hs) | Non-streaming chat, system messages, and throughput metrics. |
| `ollama-example-streaming-chat` | [`examples/StreamingChat.hs`](examples/StreamingChat.hs) | Conduit-based real-time response streaming. |
| `ollama-example-tool-calling` | [`examples/ToolCalling.hs`](examples/ToolCalling.hs) | Multi-turn tool execution loop with `toolDef` and `objectParams`. |
| `ollama-example-structured-output` | [`examples/StructuredOutput.hs`](examples/StructuredOutput.hs) | Typed record JSON schema derivation via `ToSchema` and `formatFor`. |
| `ollama-example-embeddings` | [`examples/Embeddings.hs`](examples/Embeddings.hs) | Modern batch embeddings (`/api/embed`) and legacy embeddings. |
| `ollama-example-model-management` | [`examples/ModelManagement.hs`](examples/ModelManagement.hs) | Version inspection, model listing, details, and active VRAM ps. |
| `ollama-example-generate` | [`examples/GenerateText.hs`](examples/GenerateText.hs) | Single-turn text completion, streaming, and throughput metrics. |
| `ollama-example-vision` | [`examples/Vision.hs`](examples/Vision.hs) | Multimodal chat and generation with local images (`readImageBase64`). |
| `ollama-example-logprobs` | [`examples/Logprobs.hs`](examples/Logprobs.hs) | Token log probability inspection and alternative candidate analysis. |
| `ollama-example-thinking` | [`examples/Thinking.hs`](examples/Thinking.hs) | Reasoning models with `ThinkDisabled`, `ThinkEnabled`, and `ThinkCustom`. |
| `ollama-example-model-lifecycle` | [`examples/ModelLifecycle.hs`](examples/ModelLifecycle.hs) | Model copying, creation, blob checking, and deletion lifecycle. |
| `ollama-example-conversation-store` | [`examples/ConversationStore.hs`](examples/ConversationStore.hs) | STM-backed in-memory multi-turn conversation persistence. |
| `ollama-example-client-lifecycle` | [`examples/ClientLifecycle.hs`](examples/ClientLifecycle.hs) | `clientFromEnv`, `withClient`, retry backoff, and error taxonomy. |
| `ollama-example-mcp-bridge` | [`examples/MCPBridge.hs`](examples/MCPBridge.hs) | Bidirectional conversion between Ollama tools and MCP definitions. |
| `ollama-example-system-one` | [`examples/SystemOne.hs`](examples/SystemOne.hs) | Specialized decision evaluation questions (`/v1/systemone`). |
| `ollama-example-mock-testing` | [`examples/MockTesting.hs`](examples/MockTesting.hs) | Offline unit testing with `withMockClient` without a running server. |
| `ollama-example-all-features` | [`examples/AllFeatures.hs`](examples/AllFeatures.hs) | End-to-end showcase combining multiple library features. |

Run any example with Stack:

```bash
stack run ollama-example-tool-calling
```

---

## Feature Matrix

| Feature | Haskell (`ollama-haskell`) | Official Python (`ollama-python`) | Official JS/TS (`ollama-js`) | Community Go (`ollama/ollama`) |
| :--- | :---: | :---: | :---: | :---: |
| **Strict Type Safety** | ✅ Compile-time (PVP, Smart Constructors) | ⚠️ Type hints (Runtime) | ⚠️ TypeScript (Erased at runtime) | ✅ Go Structs |
| **Response Streaming** | ✅ `conduit` ($O(1)$ constant memory) | ⚠️ Python Generator | ⚠️ Async Iterator | ⚠️ Go Channels |
| **Structured Output Derivation** | ✅ `GHC.Generics` (`ToSchema`) | ⚠️ Pydantic BaseModel | ⚠️ Zod / JSON Schema | ⚠️ Manual JSON Schema |
| **Model Context Protocol (MCP)** | ✅ Native `mcp-server` Bridge | ❌ Manual | ❌ Manual | ❌ Manual |
| **System One Decision Models** | ✅ Full ADT (`/v1/systemone`) | ✅ Supported | ❌ None | ❌ None |
| **Token Logprobs Support** | ✅ `Logprob` / `TokenLogprob` | ✅ Supported | ❌ None | ❌ None |
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

