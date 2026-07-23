# Architecture: ollama-haskell v3.0

This document describes the high-level architecture, module dependencies, and data flow of `ollama-haskell`.

---

## 1. Architectural Overview

`ollama-haskell` uses a layered, client-centric design:

```
┌─────────────────────────────────────────────────────────────┐
│                         User Code                           │
└──────────────────────────────┬──────────────────────────────┘
                               │ MonadIO m =>
                               ▼
┌─────────────────────────────────────────────────────────────┐
│                      Ollama (Umbrella)                      │
├──────────────────────────────┬──────────────────────────────┤
│    Ollama.API.Chat           │    Ollama.API.Generate       │
│    Ollama.API.Embed          │    Ollama.API.Models         │
│    Ollama.API.Blobs          │    Ollama.API.Ps             │
└──────────────────────────────┬──────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────┐
│                        Ollama.Client                        │
│                (OllamaClient, OllamaClientConfig)           │
└──────────────────────────────┬──────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────┐
│                    Ollama.Client.Internal                   │
│              (HTTP Transport, JSON Encoding, Retry)         │
└──────────────────────────────┬──────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────┐
│                    Ollama.Types & Error                     │
│    (ModelName, Digest, Message, Tool, Format, OllamaError)  │
└─────────────────────────────────────────────────────────────┘
```

---

## 2. Core Principles

1. **Client-Centric (`OllamaClient`):** All API operations accept an explicit `OllamaClient` handle carrying HTTP manager, timeout, headers, and retry configuration.
2. **Polymorphic (`MonadIO m =>`):** Every client constructor and API function is polymorphic in `MonadIO m =>` or `MonadUnliftIO m =>`.
3. **First-Class Streaming (`conduit`):** Streaming endpoints produce `ConduitT () response m ()` pipelines for efficient, memory-constant streaming.
4. **Domain Types:** Invariants are enforced via domain newtypes (`ModelName`, `Digest`, `Duration`, `Base64Image`).
5. **No Global State:** No implicit global configurations or hidden state variables.

---

## 3. Module Hierarchy

- `Ollama` — Main umbrella module re-exporting the primary API.
- `Ollama.Client` — Client construction (`defaultClient`, `newClient`, `clientFromEnv`, `withClient`).
- `Ollama.Client.Config` — `OllamaClientConfig`, `RetryPolicy`, `LogLevel`.
- `Ollama.Client.Internal` — Low-level HTTP dispatch, JSON decoding, status code handling.
- `Ollama.Error` — Structured `OllamaError` sum type.
- `Ollama.Streaming` — `HasDone` typeclass and conduit helpers.
- `Ollama.Types` — Central re-export for domain primitives (`Message`, `Tool`, `Format`, `ModelOptions`, etc.).
- `Ollama.API.*` — Submodules implementing specific API endpoints (`Chat`, `Generate`, `Embed`, `Models`, `Blobs`, `Ps`, `Version`).
