# Changelog

All notable changes to `ollama-haskell` will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP (Haskell Package Versioning Policy)](https://pvp.haskell.org/).

## [0.4.2.0] - 2026-10-25

### Added
- **Ergonomic Tool Calling & Parameter DSL (`Ollama.Types.Tool`)**:
  - Added smart constructors for zero-boilerplate tool and schema definitions: `toolDef`, `functionDef`, `objectParams`, `stringParam`, `numberParam`, `intParam`, `boolParam`, and `enumParam`.
  - Added `tcId :: Maybe Text` and `tcType :: Maybe Text` to `ToolCall` with backwards-compatible pattern synonyms (`pattern ToolCall`) and constructors (`mkToolCall`, `toolCallWithId`).
  - Added `tcfIndex :: Maybe Int` to `ToolCallFunction` with backwards-compatible pattern synonyms (`pattern ToolCallFunction`) and constructor (`mkToolCallFunction`).
  - Added `messageToolCallId :: Maybe Text` and `toolResultMessageWithId :: Text -> Text -> Text -> Message` to `Message` for multi-turn tool calling correlation.
- **Multimodal Image Helpers (`Ollama.Types.Common`)**:
  - Added `readImageBase64 :: FilePath -> IO Base64Image` to read local image files from disk and encode them as base64 without third-party boilerplate.
  - Added `encodeBase64Image :: ByteString -> Base64Image` to encode raw byte buffers.
- **Reasoning Model Response Helpers**:
  - Added `messageFullText :: Message -> Text` to `Ollama.Types.Message` (re-exported in `Ollama`).
  - Added `generateFullText :: GenerateResponse -> Text` to `Ollama.API.Generate` (re-exported in `Ollama`).
  - Both safely combine reasoning/thinking tokens (`messageThinking` / `grThinking`) and final content (`messageContent` / `grResponse`), preventing empty strings when all tokens are consumed in the thinking phase.
- **Top-Level Umbrella Re-exports (`Ollama`)**:
  - Re-exported `ModelDetails (..)`, `ShowRequest (..)`, `readImageBase64`, `encodeBase64Image`, `messageFullText`, `generateFullText`, and all tool smart constructors directly from `Ollama`.
- **System One Decision API (`Ollama.API.SystemOne`)**:
  - Added native client support for Ollama's high-speed decision evaluation endpoint (`POST /v1/systemone`), matching `ollama-python` and `ollama-rs`.
  - Supported question formats: `ChoiceQuestion` (multiple choice with class probabilities and confidence), `NoulQuestion` (binary true/false probability), and `ScoreQuestion` (ordinal scoring with expected fractional score, legend, and confidence).
  - Provided ergonomic smart constructors (`choiceQuestion`, `noulQuestion`, `noulQuestionWithCriteria`, `scoreQuestion`, `systemOneRequest`) and flexible state input via `SystemOneContent` (text or JSON `Value`).
- **Logprobs Support**:
  - Added `TokenLogprob` and `Logprob` domain types in `Ollama.Types.Common` (re-exported in `Ollama`).
  - Added `chatLogprobs :: Maybe Bool` and `chatTopLogprobs :: Maybe Int` to `ChatRequest`, and `crLogprobs :: Maybe [Logprob]` to `ChatResponse`.
  - Added `genLogprobs :: Maybe Bool` and `genTopLogprobs :: Maybe Int` to `GenerateRequest`, and `grLogprobs :: Maybe [Logprob]` to `GenerateResponse`.
- **Extended Request Convenience Methods**:
  - Added `showModelWith` to `Ollama.API.Models` to support verbose model inspection (`srqVerbose = Just True`).
  - Added `pullWith` and `pullStreamWith` to `Ollama.API.Models.Pull` for custom pull options (`insecure`, etc.).
  - Added `pushWith` and `pushStreamWith` to `Ollama.API.Models.Push` for custom push options (`insecure`, etc.).
- **Model-Defined Custom Thinking Levels**:
  - Added `ThinkCustom !Text` constructor to `ThinkingLevel` to support arbitrary reasoning effort levels (e.g. `"xhigh"`, `"ultra"`, or model-defined strings) matching official `ollama-python` v0.6.3 behavior.
- **Image Generation Diffusion Progress Tracking**:
  - Added `grCompleted :: Maybe Int` and `grTotal :: Maybe Int` to `GenerateResponse` to monitor diffusion step progression during image generation streaming.
- **Comprehensive Runnable Examples**:
  - Added 10 new runnable example executables (`ollama-example-generate`, `ollama-example-vision`, `ollama-example-logprobs`, `ollama-example-thinking`, `ollama-example-model-lifecycle`, `ollama-example-conversation-store`, `ollama-example-client-lifecycle`, `ollama-example-mcp-bridge`, `ollama-example-system-one`, `ollama-example-mock-testing`), ensuring every single library API and type is verified running against a live Ollama server.

### Fixed
- **Image Generation `GenerateResponse` JSON Parsing**:
  - Fixed a critical crash where image generation progress updates (`{ "completed": 5, "total": 20 }`) and final responses (`{ "image": "..." }`) fail to decode with `key "response" not found`. Made `response` gracefully default to empty string (`.!= ""`) when omitted by the server.
- **Tool Call `Message` JSON Parsing**:
  - Fixed JSON decoding failure when the assistant returns tool calls without a `"content"` field (or with null content) by defaulting `content` to `""`.

### Changed
- **Model Context Protocol (`Ollama.MCP`)**:
  - Enforced `mcp-server >= 0.3 && < 0.4` dependency bounds (addressing Stackage issue [#8143](https://github.com/commercialhaskell/stackage/issues/8143)).
  - Supported `SchemaRaw` constructor introduced in `mcp-server-0.3.0.0` within `mcpDefinitionToTool`.
  - Updated all Stack snapshots (`stack.yaml`, `stack-nightly.yaml`, `stack-lts-24.52.yaml`, `stack-lts-23.28.yaml`, `stack-lts-22.44.yaml`, `stack-lts-21.25.yaml`) to `mcp-server-0.3.0.0`.

---

## [0.4.1.0] - 2026-08-25

### Fixed
- **Conduit Streaming Socket Lifetime (`Ollama.Client.Internal`)**:
  - Fixed premature connection closure in `requestStreaming` by replacing `transPipe runResourceT` with exception-safe generator cleanup, ensuring streaming responses stream token-by-token across the full response without truncation.
- **Documentation & Tutorial Code Snippets**:
  - Corrected STM conversation storage documentation in `docs/tutorials/chat.markdown` and `docs/motivation.markdown` to align with the actual `Conversation` API.
  - Replaced `runConduitRes` with `runConduit` in streaming examples.
  - Fixed missing `toolName` parameter in `toolResultMessage` in `docs/tutorials/tool-calling.markdown`.
  - Corrected field names (`models` / `runningModels`) in `docs/tutorials/model-management.markdown`.
  - Fixed lazy/strict text encoding in `docs/tutorials/structured-outputs.markdown`.
  - Corrected `newMockClient` serialized `ByteString` usage in `docs/tutorials/testing.markdown`.
  - Replaced `collectStream` with genuine real-time Conduit streaming in `README.md`.

### Added
- **Model Capabilities Field (`Ollama.Types.Model`)**:
  - Added `capabilities :: !(Maybe [Text])` to `ModelInfo` and updated `FromJSON`/`ToJSON` instances to support capability discovery from `/api/tags` (e.g. `["completion", "tools", "thinking"]`).
- **Direct SchemaBuilder Re-export (`Ollama`)**:
  - Re-exported the full `SchemaBuilder` DSL (`buildSchema`, `emptyObject`, `|+`, `|++`, `|!`, `|!!`, `JsonType(..)`, `Property`, `Schema`, `objectOf`, `arrayOf`, `printSchema`) directly from the top-level `Ollama` umbrella module.
- **End-to-End Live LLM Integration Test Suite**:
  - 15 comprehensive live test cases in `test-integration/Main.hs` covering version, model inspection, non-streaming & streaming chat, structured JSON verification, tool calling round-trip, thinking models, embeddings, model lifecycle, and multi-turn STM memory persistence.
- **SDK Feature Matrix**:
  - Multi-language SDK feature matrix embedded directly in `README.md`.

### Changed
- **Dependency Cleanliness**:
  - Removed unused `resourcet` package from library `build-depends`.
- **Documentation Redesign**:
  - Completely restyled Hakyll documentation site with a minimal, restrained engineering aesthetic.

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
