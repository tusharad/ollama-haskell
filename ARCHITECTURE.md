# Architecture & Design Specifications

`ollama-haskell` is designed around three core principles:
1. **Opaque Thread-Safe Handles**: Network resources and HTTP connection managers are encapsulated within `OllamaClient`.
2. **First-Class Streaming Pipeline**: Powered by `conduit` and `resourcet` for streaming responses without memory buffering.
3. **MonadIO / MonadUnliftIO Polymorphism**: Single unified functions (`chat`, `chatStream`, `generate`) that work natively in plain `IO` as well as custom transformer stacks.

---

## Module Hierarchy

```
Ollama                              -- Public umbrella re-export module
├── Ollama.Client                   -- OllamaClient type, constructors, withClient
├── Ollama.Client.Config            -- OllamaClientConfig, RetryPolicy, LogLevel
├── Ollama.Client.Internal          -- HTTP plumbing, retry loops, streaming conduit
│
├── Ollama.API.Generate             -- /api/generate (non-streaming & streaming)
├── Ollama.API.Chat                 -- /api/chat (non-streaming & streaming)
├── Ollama.API.Embed                -- /api/embed (vector embeddings)
├── Ollama.API.Models               -- list, show, copy, delete endpoints
├── Ollama.API.Models.Create        -- /api/create
├── Ollama.API.Models.Pull          -- /api/pull
├── Ollama.API.Models.Push          -- /api/push
├── Ollama.API.Blobs                -- /api/blobs (HEAD & POST raw payload)
├── Ollama.API.Ps                   -- /api/ps (running models)
├── Ollama.API.Version              -- /api/version
│
├── Ollama.Types                    -- All public types re-exported
│   ├── Ollama.Types.Common         -- ModelName, Digest, Duration, Version, Think
│   ├── Ollama.Types.Message        -- Role, Message, userMessage, assistantMessage
│   ├── Ollama.Types.Tool           -- Tool, FunctionDef, FunctionParameters, ToolCall
│   ├── Ollama.Types.Model          -- ModelDetails, ModelInfo, ListResponse
│   ├── Ollama.Types.Options        -- ModelOptions
│   └── Ollama.Types.Format         -- Format, Schema, SchemaBuilder DSL
│
├── Ollama.Error                    -- OllamaError sum type & Exception instance
├── Ollama.Streaming                -- HasDone class, collectStream, foldStream
└── Ollama.Conversation             -- ConversationStore & STM InMemoryStore
```

---

## Technical Design Rationale

### 1. Connection Management & Manager Reuse
`OllamaClient` stores an HTTP `Manager` from `Network.HTTP.Client`. If constructed via `newClient` without providing a `configManager`, `ollama-haskell` manages the lifecycle of the TLS-enabled connection pool. `withClient` uses a bracket pattern to automatically close resources when finished.

### 2. Response Streaming Architecture
Streaming API calls (`chatStream`, `generateStream`, etc.) use `requestStreaming` in `Ollama.Client.Internal`.
- `responseOpen` opens the HTTP stream connection.
- `Data.Conduit.Binary.lines` splits line-delimited JSON chunks as ByteStrings arrive over the socket.
- `eitherDecode` parses each JSON chunk into a response value.
- `bracketP` and `transPipe runResourceT` ensure that HTTP handles are closed safely when the conduit terminates or if downstream consumers stop early.

### 3. Error Handling Hierarchy
All non-streaming API calls return `m (Either OllamaError response)`. `OllamaError` is a structured sum type:
- `HttpError HttpException` — Transport / socket errors.
- `ApiError Int Text` — Non-2xx HTTP status responses from server.
- `DecodeError Text ByteString` — Invalid JSON response payload.
- `InvalidRequest Text` — Invalid request parameter validation.
- `TimeoutError` — Network timeout.

`OllamaError` implements `Exception` so it can be thrown via `throwOllama` if desired. `isRetryable` classifies transient errors (`TimeoutError`, HTTP 5xx) for automated retry handling.

---

## How to Add a New API Endpoint

1. Create a module `Ollama.API.<Feature>` (e.g. `Ollama.API.NewFeature`).
2. Define Request and Response data types with `ToJSON` and `FromJSON` instances.
3. If streaming-capable, implement `HasDone` instance for the response type.
4. Export non-streaming functions using `request`:
   ```haskell
   newEndpoint :: MonadIO m => OllamaClient -> FeatureRequest -> m (Either OllamaError FeatureResponse)
   newEndpoint client req = request client "POST" "/api/new-feature" (Just req)
   ```
5. Re-export functions and types in `src/Ollama.hs`.
6. Add pure unit tests in `test/Test/Ollama/Unit/Types.hs`.
