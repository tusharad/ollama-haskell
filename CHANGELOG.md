# Changelog

All notable changes to `ollama-haskell` will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP (Haskell Package Versioning Policy)](https://pvp.haskell.org/).

---

## [1.0.0.0] - 2026-07-24

### Added
- **`OllamaClient` Core**: Thread-safe client handle with automatic connection manager lifecycle management (`newClient`, `defaultClient`, `clientFromEnv`, `withClient`).
- **First-Class Streaming Pipeline**: `conduit`-based response streaming (`chatStream`, `generateStream`, `pullStream`, `pushStream`, `createModelStream`).
- **Stream Combinators**: `collectStream` and `foldStream` in `Ollama.Streaming`.
- **Structured Output DSL**: `SchemaBuilder` DSL in `Ollama.Types.Format.SchemaBuilder` with operators (`|+`, `|++`, `|!`, `|!!`) for building JSON Schemas.
- **Thinking Models Support**: `Think` ADT (`ThinkEnabled`, `ThinkDisabled`, `ThinkLevel`) and `ThinkingLevel` (`ThinkLow`, `ThinkMedium`, `ThinkHigh`, `ThinkMax`) for thinking models (`qwen3.5`, `deepseek-r1`).
- **Function / Tool Calling**: `Tool`, `FunctionDef`, `FunctionParameters`, `ToolCall`, and `toolResultMessage`.
- **Environment Resolution**: URL parsing and normalization in `clientFromEnv` supporting `host:port`, `http://host:port`, and bare `host`.
- **Authorization Headers**: Support for `OLLAMA_API_KEY` bearer tokens and custom `configHeaders`.
- **Configurable Resilience**: `RetryPolicy` ADT (`NoRetry`, `ConstantRetry`, `ExponentialRetry`), lifecycle callbacks (`configOnStart`, `configOnSuccess`, `configOnError`), and `configLogger`.
- **Conversation Store**: Transactional STM-backed `InMemoryStore` and `ConversationStore` typeclass.
- **New API Modules**: `Ollama.API.Embed` (`/api/embed`), `Ollama.API.Blobs` (`/api/blobs`), `Ollama.API.Ps` (`/api/ps`), `Ollama.API.Version` (`/api/version`).
- **Testing Suite**: 28 pure unit, QuickCheck property, and golden tests, plus live server integration test suite.

### Changed
- **MonadIO / MonadUnliftIO Polymorphism**: All API functions use `MonadIO m =>` / `MonadUnliftIO m =>` signatures instead of dual `*M` variants.
- **Typed Newtypes**: `ModelName`, `Digest`, `Base64Image`, `Duration`, `Version` replace primitive types.
- **Error Types**: `OllamaError` sum type with structured constructors and `Exception` instance.

### Deprecated / Removed
- Removed legacy callback-tuple streaming API in favor of `conduit`.
- Removed global `Maybe OllamaConfig` state.
