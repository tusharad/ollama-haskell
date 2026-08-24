---
title: Multi-Language SDK Comparison
category: Deep Dives
description: Comprehensive feature matrix comparing ollama-haskell with Python, JavaScript/TypeScript, and Go SDKs.
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

## Detailed Architectural Comparison

### 1. Memory Safety & Streaming

- **`ollama-haskell`**: Uses `conduit` streams which process chunks incrementally. If a downstream consumer cancels or encounters an exception, connection resources are finalized immediately with `ResourceT`.
- **Python / JS SDKs**: Often buffer entire JSON payloads into memory when parsing complex tool calls or large context windows, causing memory spikes.

### 2. Structured Outputs & Type Safety

- **`ollama-haskell`**: Type declarations (`data Weather = ... deriving (Generic, ToSchema)`) produce exact JSON schema definitions without requiring external schema generators or runtime reflection.
- **Python / JS**: Rely on heavy third-party dependencies (Pydantic / Zod) that add runtime overhead and potential schema drift.
