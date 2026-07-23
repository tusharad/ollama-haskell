# PRD: ollama-haskell v1.0 — Industry-Grade Haskell Client for Ollama

> **Version:** 1.0  
> **Status:** Draft  
> **Author:** Tushar Adhatrao  
> **Last Updated:** 2026-07-23  

---

## 1. Vision & Mission

**Vision:** Make `ollama-haskell` the gold-standard Haskell client for Ollama — fully type-safe, ergonomic, battle-tested, and a model for how Haskell libraries should be built.

**Mission:** Deliver a client library that:
- Has **100% Ollama API coverage** including experimental endpoints.
- Leverages Haskell's type system to make **illegal API states unrepresentable**.
- Follows established Haskell library best practices (`lens`, `mtl`-style, `conduit`/`streaming` for streams).
- Is **production-ready** with comprehensive error handling, retry logic, connection pooling, and logging.
- Has **best-in-class documentation**, tests, benchmarks, and CI/CD.

---

## 2. Current State Analysis

### 2.1 What ollama-haskell v0.2.1 Does Well
- Covers most core API endpoints (generate, chat, embeddings, model management).
- Has a `SchemaBuilder` DSL for structured outputs.
- Has validation for request types (`validateChatOps`, `validateGenerateOps`).
- Good haddock documentation with examples.
- Has `MonadIO` lifted variants (`chatM`, `generateM`, etc.).
- Has a `ConversationStore` typeclass with in-memory STM implementation.
- Has lifecycle hooks (`onModelStart`, `onModelFinish`, `onModelError`).
- Has retry logic with configurable delay.
- Test suite covering most endpoints.

### 2.2 Gaps Identified (Compared to ollama-python, ollama-rs, ollama4j)

| Gap | ollama-python | ollama-rs | ollama4j | ollama-haskell |
|-----|:---:|:---:|:---:|:---:|
| Async client (full async API) | ✅ `AsyncClient` | ✅ `tokio` | ✅ `CompletableFuture` | ❌ Only `MonadIO` lift |
| Builder pattern for client | ✅ | ✅ `OllamaBuilder` | ✅ | ❌ Raw record update |
| Streaming via first-class abstractions | ✅ Iterators | ✅ `tokio_stream` | ✅ `Stream<>` | ❌ Callback tuples |
| `OLLAMA_HOST` env var support | ✅ | ✅ | ✅ | ❌ |
| `OLLAMA_API_KEY` / auth headers | ✅ | ✅ `headers` feature | ✅ | ❌ |
| Custom request headers | ✅ | ✅ | ✅ | ❌ |
| Connection pooling (manager reuse) | ✅ (httpx) | ✅ (reqwest) | ✅ (OkHttp) | ⚠️ Partial (`commonManager`) |
| `conduit`/`streaming` integration | N/A | ✅ `Stream` | N/A | ❌ |
| Property-based tests (QuickCheck/Hedgehog) | ❌ | ❌ | ❌ | ❌ |
| Pure unit tests (no server) | ⚠️ Mocked | ⚠️ | ✅ | ❌ All integration |
| Benchmarks | ❌ | ❌ | ❌ | ❌ |
| `think` as sum type (`Bool` or level) | ✅ | ❌ | ❌ | ❌ `Maybe Bool` only |
| Image generation (experimental) | ❌ | ❌ | ❌ | ❌ |
| `tool_name` in messages | ✅ | ❌ | ❌ | ❌ |
| `renderer`/`parser` in create | ✅ | ❌ | ❌ | ❌ |
| `Eq` for `OllamaError` (non-trivial) | N/A | N/A | N/A | ⚠️ Broken pattern |
| Logging integration | ❌ | ❌ | ✅ SLF4J | ❌ |
| `cabal-version: 2.4+` | N/A | N/A | N/A | ❌ Uses 1.12 |
| Deprecation of old `/api/embeddings` | ✅ | ✅ | ✅ | ❌ Still present conceptually |
| `FromJSON` for request types (roundtrip) | ⚠️ | ⚠️ | ⚠️ | ❌ |
| `draft_num_predict` option | ✅ (via API) | ❌ | ❌ | ❌ |

---

## 3. Target Architecture

### 3.1 Module Hierarchy (v1.0)

```
Ollama                              -- Top-level re-export module
Ollama.Client                       -- OllamaClient type, builder, smart constructors
Ollama.Client.Config                -- OllamaConfig, env var resolution
Ollama.Client.Internal              -- HTTP plumbing, streaming internals

Ollama.API.Generate                 -- /api/generate
Ollama.API.Chat                     -- /api/chat  
Ollama.API.Embed                    -- /api/embed (new), /api/embeddings (deprecated)
Ollama.API.Models                   -- list, show, copy, delete
Ollama.API.Models.Create            -- /api/create
Ollama.API.Models.Pull              -- /api/pull
Ollama.API.Models.Push              -- /api/push
Ollama.API.Blobs                    -- /api/blobs
Ollama.API.Ps                       -- /api/ps
Ollama.API.Version                  -- /api/version

Ollama.Types                        -- All public types re-exported
Ollama.Types.Message                -- Message, Role
Ollama.Types.Tool                   -- InputTool, ToolCall, FunctionDef, etc.
Ollama.Types.Model                  -- ModelInfo, ModelDetails, RunningModel
Ollama.Types.Options                -- ModelOptions
Ollama.Types.Format                 -- Format, Schema, SchemaBuilder DSL
Ollama.Types.Common                 -- Version, Duration, etc.

Ollama.Error                        -- OllamaError (structured, Exception instance)
Ollama.Streaming                    -- Conduit/streaming-based streaming API
Ollama.Conversation                 -- ConversationStore typeclass + InMemory impl

Ollama.Testing                      -- Mock server, test helpers (exported for users)
```

### 3.2 Core Design Principles

1. **Client-centric architecture**: All API calls go through an `OllamaClient` value (like `ollama-rs`'s `Ollama` struct and `ollama-python`'s `Client`).
2. **No implicit global state**: Remove `Maybe OllamaConfig` from every function. The client carries its config.
3. **MonadIO / MonadUnliftIO polymorphism**: All API functions and client constructors use `MonadIO m =>` (or `MonadUnliftIO m =>` for resource/streaming operations). This provides a single, unified API that works natively in plain `IO` as well as any application monad transformer stack (Servant handlers, RIO, `ReaderT`, custom App monad) without requiring dual `*M` function variants (like the legacy `chat` vs `chatM`).
4. **Streaming as first-class**: Use `conduit` for streaming endpoints (`ConduitT () response m ()`), not callback tuples.
5. **Type-safe request builders**: Use smart constructors and phantom types where it helps, not just record syntax.
6. **Newtypes for domain concepts**: `ModelName`, `Digest`, `Base64Image`, `Duration` — not raw `Text`/`Int64`.
7. **Errors are typed and structured**: `OllamaError` with proper `Exception` instance, `HasCallStack` for debugging.

---

## 4. Functional Requirements

### 4.1 Full API Coverage

Every endpoint from `api.md` must be implemented:

| Endpoint | Method | Current | v1.0 Target |
|----------|--------|:-------:|:-----------:|
| `POST /api/generate` | Generate completion | ✅ | ✅ + image gen params |
| `POST /api/chat` | Chat completion | ✅ | ✅ + `tool_name` field |
| `POST /api/create` | Create model | ✅ | ✅ + `renderer`, `parser` |
| `GET /api/tags` | List models | ✅ | ✅ |
| `POST /api/show` | Show model info | ✅ | ✅ |
| `POST /api/copy` | Copy model | ✅ | ✅ |
| `DELETE /api/delete` | Delete model | ✅ | ✅ |
| `POST /api/pull` | Pull model | ✅ | ✅ |
| `POST /api/push` | Push model | ✅ | ✅ |
| `POST /api/embed` | Generate embeddings | ✅ | ✅ |
| `POST /api/embeddings` | (deprecated) | ⚠️ | ✅ with deprecation warning |
| `GET /api/ps` | Running models | ✅ | ✅ |
| `HEAD /api/blobs/:digest` | Check blob | ✅ | ✅ |
| `POST /api/blobs/:digest` | Push blob | ✅ | ✅ |
| `GET /api/version` | Server version | ✅ | ✅ |

### 4.2 `think` Parameter

The `think` parameter should support both `Bool` and thinking level:

```haskell
data Think
  = ThinkEnabled
  | ThinkDisabled
  | ThinkLevel ThinkingLevel

data ThinkingLevel = Low | Medium | High | Max
```

### 4.3 Streaming API

Replace callback tuples with first-class streaming:

```haskell
-- Non-streaming (polymorphic over MonadIO m)
generate :: MonadIO m => OllamaClient -> GenerateRequest -> m (Either OllamaError GenerateResponse)

-- Streaming via ConduitT (polymorphic over MonadIO m / MonadUnliftIO m)
generateStream :: MonadIO m => OllamaClient -> GenerateRequest -> ConduitT () GenerateResponse m ()
```

### 4.4 Client Builder

```haskell
data OllamaClient  -- abstract, not exported constructor

-- Smart constructor with defaults
defaultClient :: IO OllamaClient

-- Builder
newClient :: OllamaClientConfig -> IO OllamaClient

-- From environment
clientFromEnv :: IO OllamaClient  -- reads OLLAMA_HOST, OLLAMA_API_KEY

data OllamaClientConfig = OllamaClientConfig
  { baseUrl      :: !BaseUrl          -- default: http://127.0.0.1:11434
  , manager      :: !(Maybe Manager)  -- optional shared HTTP manager
  , timeout      :: !NominalDiffTime  -- default: 90s
  , retryPolicy  :: !RetryPolicy      -- default: noRetry
  , headers      :: ![(HeaderName, ByteString)]
  , logger       :: !(Maybe (LogLevel -> Text -> IO ()))
  , apiKey       :: !(Maybe Text)     -- for OLLAMA_API_KEY
  }
```

### 4.5 Newtypes & Smart Constructors

```haskell
newtype ModelName = ModelName { unModelName :: Text }
  deriving (Eq, Ord, Show, IsString, ToJSON, FromJSON)

newtype Digest = Digest { unDigest :: Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

newtype Base64Image = Base64Image { unBase64Image :: ByteString }
  deriving (Eq, Show, ToJSON, FromJSON)

-- Duration is in nanoseconds from Ollama
newtype Duration = Duration { unDurationNanos :: Int64 }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

durationToSeconds :: Duration -> Double
durationToMillis  :: Duration -> Double
```

### 4.6 Error Handling

```haskell
data OllamaError
  = HttpError !HttpException
  | ApiError !StatusCode !Text           -- HTTP status + body
  | DecodeError !Text !ByteString        -- message + raw response
  | TimeoutError
  | InvalidRequest !Text                 -- client-side validation
  | ConnectionError !Text                -- cannot reach server
  deriving (Show, Typeable)

instance Exception OllamaError

-- Proper Eq via pattern matching (fix current broken Eq)
instance Eq OllamaError where ...
```

### 4.7 Message & Role Types

```haskell
data Role = System | User | Assistant | Tool
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

data Message = Message
  { messageRole      :: !Role
  , messageContent   :: !Text
  , messageImages    :: !(Maybe [Base64Image])
  , messageToolCalls :: !(Maybe [ToolCall])
  , messageToolName  :: !(Maybe Text)        -- NEW: from API spec
  , messageThinking  :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

-- Smart constructors (keep existing ones, add toolResultMessage)
userMessage       :: Text -> Message
systemMessage     :: Text -> Message
assistantMessage  :: Text -> Message
toolMessage       :: Text -> Message
toolResultMessage :: Text -> Text -> Message  -- content + tool_name
```

---

## 5. Non-Functional Requirements

### 5.1 Performance
- **Connection pooling**: Reuse HTTP `Manager` across requests by default.
- **Lazy streaming**: Stream responses without buffering the entire body.
- **Strict fields**: All record fields must be strict (`!`).
- **No unnecessary intermediate allocations**: Use `ByteString` builders for request bodies.

### 5.2 Reliability
- **Configurable retry policies**: Exponential backoff, max retries, retry-on predicates.
- **Graceful timeout handling**: Configurable per-request timeouts.
- **Resource cleanup**: Proper bracket/finally patterns for HTTP connections.

### 5.3 Observability
- **Structured logging**: Optional logger callback with levels (Debug, Info, Warn, Error).
- **Request/response metadata**: Expose timing information from Ollama responses.
- **Tokens-per-second calculation**: Utility functions for perf metrics.

### 5.4 Compatibility
- **GHC support**: GHC 9.4+ (drop support for GHC < 9.4 since no backward compat needed).
- **cabal-version**: Upgrade to `3.0`.
- **Stackage**: Target inclusion in latest LTS.
- **Platform**: Linux, macOS, Windows.

---

## 6. Library Best Practices Checklist

> These are the non-negotiable standards that any production-grade Haskell library must follow.

### 6.1 API Design
- [ ] **Abstract data types**: Export types but not their constructors where invariants must be maintained.
- [ ] **Smart constructors**: Provide `mk*` or builder functions that validate inputs.
- [ ] **Newtypes for domain concepts**: Never use raw `Text` or `Int` where a domain type applies.
- [ ] **Avoid boolean blindness**: Use sum types instead of `Bool` parameters.
- [ ] **`NonEmpty` for non-empty collections**: Already used for messages — apply everywhere.
- [ ] **No orphan instances**: All instances defined in the module that defines the type.
- [ ] **Explicit export lists**: Every module must have one (already enforced via `-Wmissing-export-lists`).
- [ ] **No partial functions**: No `head`, `tail`, `fromJust`, `read`, `!!` in library code.
- [ ] **Minimal dependencies**: Keep dependency footprint small. Prefer `base` utilities.

### 6.2 Documentation
- [ ] **Haddock on every exported symbol**: No exceptions.
- [ ] **Module-level documentation**: Every module has a purpose description.
- [ ] **`@since` annotations**: On every new public API addition.
- [ ] **Doctests**: Runnable examples in Haddock comments via `doctest`.
- [ ] **README with quickstart**: Get from `cabal install` to first API call in < 5 lines.
- [ ] **CHANGELOG**: Follows [Keep a Changelog](https://keepachangelog.com/) format.
- [ ] **CONTRIBUTING.md**: Development setup, coding standards, PR process.
- [ ] **ARCHITECTURE.md**: High-level module design and data flow.

### 6.3 Testing
- [ ] **Pure unit tests**: JSON roundtrip, validation, serialization (no server needed).
- [ ] **Integration tests**: Against a real Ollama server (opt-in via flag/env var).
- [ ] **Property-based tests**: `QuickCheck` or `Hedgehog` for serialization roundtrips and invariant checks.
- [ ] **Golden tests**: Snapshot testing for JSON serialization output.
- [ ] **Test coverage**: Aim for >80% line coverage on pure code paths.
- [ ] **Test isolation**: Each test is independent; no shared mutable state between tests.
- [ ] **CI-friendly**: Tests run in CI without an Ollama server (pure + mocked tests).

### 6.4 Build & CI
- [ ] **Warnings as errors in CI**: `-Werror` in CI, `-Wall -Wcompat` always.
- [ ] **GHC version matrix**: Test against at least 2 GHC versions.
- [ ] **HLint**: Zero warnings policy.
- [ ] **Fourmolu/Ormolu**: Consistent formatting, enforced in CI.
- [ ] **Cabal check**: `cabal check` must pass with zero warnings.
- [ ] **Hackage-ready**: `cabal sdist` produces a clean tarball.
- [ ] **GitHub Actions**: Build, test, lint, format check, Hackage upload (on tag).

### 6.5 Error Handling
- [ ] **Sum type errors**: Structured `OllamaError` covering all failure modes.
- [ ] **`Exception` instance**: For use with `throwIO`/`catch`.
- [ ] **No `error`/`undefined` in library code**: Use `Either` or `Maybe` instead.
- [ ] **`HasCallStack` constraints**: On functions that can fail, for better error traces.
- [ ] **User-facing error messages**: Every error variant produces a human-readable `show`.

### 6.6 Performance & Safety
- [ ] **Strict fields**: `!` on all record fields to avoid space leaks.
- [ ] **`UNPACK` pragmas**: On primitive types in hot paths.
- [ ] **No lazy I/O**: Use strict `ByteString` or streaming for I/O.
- [ ] **Resource safety**: `bracket` for HTTP managers and connections.
- [ ] **Thread safety**: Document thread-safety guarantees for the client.

### 6.7 Package Metadata
- [ ] **Synopsis & description**: Clear, concise, Hackage-friendly.
- [ ] **Proper license file**: MIT license included.
- [ ] **Bug tracker URL**: Points to GitHub issues.
- [ ] **Source repository**: Points to GitHub.
- [ ] **Stability field**: `experimental` → `stable` at v1.0.
- [ ] **Tested-with field**: Lists supported GHC versions.
- [ ] **Category**: `Web`, `AI`, `Network`.
- [ ] **Extra-doc-files**: Include README and CHANGELOG for Hackage.

---

## 7. User Personas & Use Cases

### Persona 1: Application Developer
> "I want to call Ollama from my Haskell web server to add AI features."

- Needs: Simple API, connection pooling, streaming for SSE, structured outputs.
- Example: `chat client (chatRequest "llama3.2" [userMessage "Hello"])`

### Persona 2: AI/ML Researcher
> "I want to generate embeddings and do batch processing from Haskell."

- Needs: Batch embedding API, model management (pull/push), performance metrics.
- Example: `embed client (embedRequest "nomic" ["text1", "text2", "text3"])`

### Persona 3: DevOps / Tooling Developer
> "I want to manage Ollama models programmatically in my deployment scripts."

- Needs: Model CRUD operations, blob management, progress streaming for pulls.
- Example: `pullStream client "llama3.2" (\progress -> logProgress progress)`

### Persona 4: Library Author
> "I want to build higher-level abstractions on top of ollama-haskell."

- Needs: Clean type hierarchy, re-exportable types, abstract `OllamaClient`, mock-friendly design.
- Example: Uses `Ollama.Testing` module for test infrastructure.

---

## 8. Migration from v0.2 to v1.0

Since backward compatibility is explicitly **not** a goal, the v1.0 release is a clean break:

| `chat ops (Just config)` | `chat client request` |
| `Maybe OllamaConfig` parameter | Config baked into `OllamaClient` |
| `chat` (IO) vs `chatM` (MonadIO) | Unified single `chat` polymorphic in `MonadIO m =>` |
| `stream :: Maybe (a -> IO (), IO ())` | `chatStream :: Client -> Req -> ConduitT () Resp m ()` |
| `GenerateOps { modelName = "..." }` | `generateRequest (ModelName "...") "prompt"` |
| `Data.Ollama.*` module namespace | `Ollama.*` module namespace |
| `think :: Maybe Bool` | `think :: Maybe Think` (sum type) |
| `Int64` durations | `Duration` newtype |
| `Text` model names | `ModelName` newtype |

---

## 9. Success Metrics

| Metric | Target |
|--------|--------|
| API endpoint coverage | 100% |
| Haddock coverage | 100% of exports |
| Pure test coverage | >80% line coverage |
| HLint warnings | 0 |
| Build time (clean) | <60s |
| Hackage candidate upload | Successful |
| CI pipeline (GHC matrix) | Green on 9.4, 9.6, 9.8, 9.10 |
| Dependency count (direct) | ≤15 |
| Zero uses of partial functions | Verified by HLint rules |

---

## 10. Out of Scope for v1.0

- OpenAI-compatible API endpoint support.
- Web search / web fetch endpoints (experimental, not in core API).
- Persistent conversation store backends (SQLite, Redis) — typeclass is provided, backends are separate packages.
- GUI or CLI tool built on top of the library.
- Auto-generated client from OpenAPI spec.
