---
title: API Module Map & Reference
category: Reference
description: High-level overview and sitemap of all exposed library modules in ollama-haskell.
---

## Overview

The `ollama-haskell` library is organized into clean, focused sub-modules. You can import everything from the umbrella module `Ollama`, or import granular modules as needed.

Full Haddock documentation is hosted on [Hackage :: ollama-haskell](https://hackage.haskell.org/package/ollama-haskell).

---

## Exposed Modules

### 1. Core & Umbrella
- **`Ollama`**: Umbrella re-export module exposing client constructors, API endpoints, types, streaming combinators, and MCP integration.

### 2. Client & Configuration
- **`Ollama.Client`**: `OllamaClient` handle management (`newClient`, `defaultClient`, `clientFromEnv`, `withClient`, `closeClient`).
- **`Ollama.Client.Config`**: Configuration records (`OllamaClientConfig`), retry policies (`RetryPolicy`, `ExponentialRetry`, `ConstantRetry`), and logging callbacks (`LogLevel`).

### 3. API Endpoints
- **`Ollama.API.Chat`**: Chat completions (`chat`, `chatStream`, `ChatRequest`, `ChatResponse`).
- **`Ollama.API.Generate`**: Raw text generation (`generate`, `generateStream`, `GenerateRequest`, `GenerateResponse`).
- **`Ollama.API.Embed`**: High-performance vector embeddings (`embed`, `EmbedRequest`, `EmbedResponse`).
- **`Ollama.API.Models`**: Model inspection and operations (`listModels`, `showModel`, `copyModel`, `deleteModel`).
- **`Ollama.API.Models.Pull`**: Pull models with progress tracking (`pull`, `pullStream`).
- **`Ollama.API.Models.Push`**: Push models to registries (`push`, `pushStream`).
- **`Ollama.API.Models.Create`**: Create models from Modelfiles (`createModel`, `createModelStream`).
- **`Ollama.API.Blobs`**: Manage binary blob digests (`checkBlob`, `pushBlob`).
- **`Ollama.API.Ps`**: Check active models in GPU VRAM (`listRunning`).
- **`Ollama.API.Version`**: Query server version (`getVersion`).

### 4. Types & Formats
- **`Ollama.Types`**: Core data types re-exports.
- **`Ollama.Types.Message`**: Messages (`Message`, `Role`, `userMessage`, `systemMessage`, `assistantMessage`, `toolMessage`, `toolResultMessage`, `imageMessage`).
- **`Ollama.Types.Tool`**: Function definitions (`Tool`, `FunctionDef`, `FunctionParameters`, `ToolCall`).
- **`Ollama.Types.Format.SchemaDerive`**: Generic JSON schema derivation (`ToSchema`, `ToJsonType`, `schemaFor`, `formatFor`).
- **`Ollama.Types.Format.SchemaBuilder`**: Manual schema construction DSL (`SchemaBuilder`, `|+`, `|++`, `|!`, `|!!`).
- **`Ollama.Types.Common`**: Smart constructors and newtypes (`ModelName`, `Think`, `ThinkingLevel`, `Duration`, `Digest`).

### 5. Advanced Integrations
- **`Ollama.MCP`**: Model Context Protocol integration with `mcp-server` (`toolToMcpDefinition`, `mcpDefinitionToTool`, `runMcpServerStdio`, `runMcpServerHttp`).
- **`Ollama.Streaming`**: Stream helpers (`collectStream`, `foldStream`).
- **`Ollama.Conversation`**: STM-backed transactional chat store (`InMemoryStore`, `ConversationStore`).
- **`Ollama.Testing`**: Pure mock testing harness (`newMockClient`, `mockChatResponse`, `mockGenerateResponse`, `mockEmbedResponse`).
- **`Ollama.Error`**: Unified error sum type (`OllamaError`, `isRetryable`, `throwOllama`).
