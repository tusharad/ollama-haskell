# SDK Feature & Architecture Comparison

This document provides a comprehensive technical comparison between **`ollama-haskell` (v0.3.0.0)**, official SDKs in other ecosystems (**Python**, **JavaScript/TypeScript**, **Go**), and existing Haskell LLM libraries.

---

## 1. Feature Support Matrix Across Language SDKs

| Feature / Endpoint | `ollama-haskell` (v0.3.0.0) | `ollama-python` | `ollama-js` | `ollama/ollama/api` (Go) |
| :--- | :---: | :---: | :---: | :---: |
| **Language Paradigm** | Pure Functional / Typed | Dynamic / Async | Dynamic / Promises | Imperative / Structs |
| **Response Streaming** | `conduit` Pipelines | Iterator / AsyncGenerator | AsyncIterable | Channel / Callback |
| **Chat Completions (`/api/chat`)** | ✅ | ✅ | ✅ | ✅ |
| **Text Generation (`/api/generate`)** | ✅ | ✅ | ✅ | ✅ |
| **Thinking / Reasoning Models** | ✅ `Think` ADT (`qwen3.5`, `deepseek-r1`) | ⚠️ Raw JSON parameter | ⚠️ Raw JSON parameter | ⚠️ Raw JSON parameter |
| **Structured Output Schema** | ✅ Type-safe `SchemaBuilder` DSL | ⚠️ Pydantic / Raw Dict | ⚠️ Zod / Raw Schema | ⚠️ Struct tag / Raw Schema |
| **Function / Tool Calling** | ✅ Strongly-typed `Tool` ADT | ✅ Dict / Callables | ✅ JSON schema objects | ✅ Go Structs |
| **Vector Embeddings (`/api/embed`)** | ✅ | ✅ | ✅ | ✅ |
| **Model Lifecycle (`copy`, `delete`, `show`, `ps`)** | ✅ | ✅ | ✅ | ✅ |
| **Blob Management (`checkBlob`, `pushBlob`)** | ✅ | ✅ | ✅ | ✅ |
| **Automatic Retry Policy** | ✅ `NoRetry`, `Constant`, `Exponential` | ❌ (User implementation) | ❌ (User implementation) | ❌ (User implementation) |
| **Connection Lifecycles** | ✅ Automatic GC & Bracket | ⚠️ Manual session | ⚠️ Fetch client | ⚠️ Manual `http.Client` |
| **STM Session Management** | ✅ `InMemoryStore` & `ConversationStore` | ❌ | ❌ | ❌ |
| **Mock Testing Harness** | ✅ `Ollama.Testing` (`withMockClient`) | ❌ | ❌ | ❌ |

---

## 2. Comparison with Other Haskell LLM Libraries

| Feature | `ollama-haskell` | `openai-hs` | `langchain-hs` |
| :--- | :--- | :--- | :--- |
| **Target Provider** | Local Ollama Engine | OpenAI Cloud API | Multi-provider Framework |
| **Streaming Abstraction** | First-class `conduit` streams | Lazy ByteString / SSE | Custom Stream types |
| **Retry & Resilience** | Integrated `retry` policy backoff | None | Basic |
| **Mocking & Testing** | Built-in `Ollama.Testing` module | None | Mock handlers |
| **Memory Persistence** | STM-backed `InMemoryStore` | Manual | Vector store abstractions |

---

## 3. Key Architectural Advantages of `ollama-haskell`

1. **Type Safety & Smart Constructors**:
   - String parameters are wrapped in domain-specific newtypes (`ModelName`, `Digest`, `Base64Image`, `Duration`, `Version`).
   - `mkModelName` validates model name invariants at runtime.

2. **First-Class Streaming via `conduit`**:
   - `chatStream` and `generateStream` stream line-delimited JSON objects over constant memory pipelines without buffering entire responses in memory.
   - Stream combinators `collectStream` and `foldStream` simplify consumption.

3. **Domain-Specific Schema Builder (`SchemaBuilder`)**:
   - Construct complex JSON Schemas using readable infix operators:
     ```haskell
     userSchema = buildSchema $
       emptyObject
         |+ ("name", JString)
         |+ ("age", JInteger)
         |! "name"
     ```

4. **Built-In Resilient Transport**:
   - Exponential backoff retry strategies (`ExponentialRetry 3 1.0`).
   - Lifecycle callbacks (`configOnStart`, `configOnSuccess`, `configOnError`).
   - Customizable logger thresholds (`Debug`, `Info`, `Warn`, `Error`).

5. **Mock Infrastructure (`Ollama.Testing`)**:
   - Test application code deterministically offline using `withMockClient` without needing a running Ollama server during CI unit testing.
