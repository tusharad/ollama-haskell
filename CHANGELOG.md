# Changelog

All notable changes to `ollama-haskell` will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP (Haskell Package Versioning Policy)](https://pvp.haskell.org/).

---

## [0.4.0.0] - 2026-08-20

### Added
- **Model Context Protocol (MCP) Integration (`Ollama.MCP`)**:
  - Full bidirectional integration with the Hackage `mcp-server` package (`mcp-server >= 0.2 && < 0.3`).
  - Seamless conversion between Ollama function calling definitions (`Tool`, `ToolCall`) and MCP definitions (`ToolDefinition`, `ArgumentDefinition`, `Content`, `McpSchema`).
  - Bridge functions: `toolToMcpDefinition`, `mcpDefinitionToTool`, `toolCallToMcpArgs`, `mcpContentToToolOutput`.
  - Re-exported MCP server runners (`runMcpServerStdio`, `runMcpServerHttp`, `runMcpServerHttpWithConfig`).
  - Dedicated unit test suite in `Test.Ollama.Unit.MCP`.
- **Automatic JSON Schema Derivation (`Ollama.Types.Format.SchemaDerive`)**:
  - Typeclasses `ToSchema` and `ToJsonType` enabling generic derivation of JSON schemas directly from Haskell record types via `GHC.Generics`.
  - Smart handling of optional fields (`Maybe a` omitted from `required`), nested records (`JObject`), lists (`JArray`), and simple sum enums (`string` enum).
  - Helper functions `schemaFor` and `formatFor` for effortless integration with `chat` / `generate` structured outputs.
  - Dedicated unit test suite in `Test.Ollama.Unit.SchemaDerive`.
- **Configurable Client Timeout**:
  - Support for custom request timeout intervals in `OllamaClientConfig` (`configTimeout`).

### Changed
- **PVP Compliance & Upper Bounds**:
  - Added strict upper bounds for `network-uri` (`>= 2.6 && < 2.8`) and `mcp-server` (`>= 0.2 && < 0.3`).
  - Upgraded Stack resolvers and snapshot dependencies (`lts-21.25`, `lts-22.44`, `lts-23.28`, `lts-24.52`, `nightly`).

---

## [0.3.0.0] - 2026-08-04

### Added
- **`OllamaClient` Core**: Thread-safe client handle with automatic connection manager lifecycle management (`newClient`, `defaultClient`, `clientFromEnv`, `withClient`).
- **First-Class Streaming Pipeline**: `conduit`-based response streaming (`chatStream`, `generateStream`, `pullStream`, `pushStream`, `createModelStream`).
- **Stream Combinators**: `collectStream` and `foldStream` in `Ollama.Streaming`.
- **Structured Output DSL**: Type-safe `SchemaBuilder` DSL in `Ollama.Types.Format.SchemaBuilder` (`|+`, `|++`, `|!`, `|!!`) for constructing JSON Schemas.
- **Thinking / Reasoning Models Support**: `Think` ADT (`ThinkEnabled`, `ThinkDisabled`, `ThinkLevel`) and `ThinkingLevel` (`ThinkLow`, `ThinkMedium`, `ThinkHigh`, `ThinkMax`) supporting models such as `qwen3.5` and `deepseek-r1`.
- **Function / Tool Calling**: `Tool`, `FunctionDef`, `FunctionParameters`, `ToolCall`, and `toolResultMessage` helper.
- **Environment Resolution**: Automatic `OLLAMA_HOST` parsing and normalization in `clientFromEnv` supporting `host:port`, `http://host:port`, and bare `host`.
- **Authorization & Headers**: Support for `OLLAMA_API_KEY` bearer tokens and custom `configHeaders`.
- **Configurable Resilience**: `RetryPolicy` ADT (`NoRetry`, `ConstantRetry`, `ExponentialRetry`), lifecycle callbacks (`configOnStart`, `configOnSuccess`, `configOnError`), and structured logger `configLogger`.
- **Token Throughput Metrics**: Metrics helpers `chatEvalTokensPerSecond`, `chatPromptEvalTokensPerSecond`, `evalTokensPerSecond`, `promptEvalTokensPerSecond`, `tokensPerSecond`.
- **Testing Infrastructure**: Built-in mock testing module `Ollama.Testing` (`newMockClient`, `withMockClient`, `mockGenerateResponse`, `mockChatResponse`, `mockEmbedResponse`, `mockListModelsResponse`).
- **Conversation Store**: Transactional STM-backed `InMemoryStore` and `ConversationStore` typeclass.
- **New API Endpoints**: `Ollama.API.Embed` (`/api/embed`), `Ollama.API.Blobs` (`/api/blobs`), `Ollama.API.Ps` (`/api/ps`), `Ollama.API.Version` (`/api/version`).
- **Benchmark Suite**: Criterion/tasty-bench suite in `bench/Main.hs` measuring serialization and throughput.

### Changed
- **MonadIO / MonadUnliftIO Polymorphism**: All API functions use `MonadIO m =>` / `MonadUnliftIO m =>` signatures instead of dual `*M` variants.
- **Typed Newtypes**: `ModelName`, `Digest`, `Base64Image`, `Duration`, `Version` replace primitive string types.
- **Unified Error Type**: `OllamaError` sum type with structured constructors and `Exception` instance.

### Deprecated
- `embeddings` endpoint (`/api/embeddings`) marked deprecated in favor of `/api/embed`.
