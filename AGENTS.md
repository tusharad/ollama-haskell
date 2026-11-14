# AGENTS.md — ollama-haskell

This file is the primary orientation document for AI coding agents working on this codebase.
Read this fully before making any change. Update it when architectural decisions are made.

---

## What This Project Is

`ollama-haskell` is an industry-grade, type-safe Haskell client library for the [Ollama](https://ollama.com) local and cloud LLM engine.

**Current state**: Released through `v0.4.2.0`, currently stabilizing `v0.4.3.0`.
**Active target**: `v0.4.3.0` (Stream error propagation & accumulators, Ollama Cloud Web Grounding, VRAM lifecycle management, type-safe structured outputs, and autonomous multi-turn tool loops).

The `v0.4.3.0` product roadmap and user stories live at:
`wiki/user-stories-0.4.3.0.md` (or the `roadmap-0.4.3.0` artifact).

---

## Formal Verification Requirement

**Every new feature or bug fix MUST follow the `formal-spec-verifier` skill protocol:**

1. Read `.agents/skills/formal-spec-verifier/SKILL.md`
2. Fill a System Law Spec from `.agents/skills/formal-spec-verifier/resources/system-law-template.yaml`
3. Write Tasty / HSpec / QuickCheck properties FIRST (leave implementation body as `undefined` until contract and tests are in place)
4. Implement minimum code to satisfy the oracle
5. Run: `./.agents/skills/formal-spec-verifier/scripts/verify_gate.sh --haskell`

---

## Branching Strategy & Git Protocol

All agents and contributors working on this codebase MUST follow this branching model without exception:

### 1. Branch Roles & Hierarchy
- **`main`**: Protected production branch. Represents verified, stable releases tagged with SemVer (`vX.Y.Z`). **NEVER commit or push directly to `main`.**
- **`develop`**: Primary integration branch and default development target. All feature and bugfix branches originate here and merge back here.
- **Supporting Branches**:
  - `feat/<story-or-feature>` (e.g., `feat/web-grounding`, `feat/structured-outputs`): Cut from `develop`, merged back to `develop` via PR.
  - `fix/<bug-id-or-description>` (e.g., `fix/stream-cancellation`): Cut from `develop`, merged back to `develop`.
  - `release/vX.Y.Z`: Release stabilization branches cut from `develop` when preparing milestone releases; merged into `main` (with release tag) and back into `develop`.
  - `hotfix/<description>`: Emergency production fixes cut from `main`, merged into both `main` and `develop`.
  - `chore/<task>`: Documentation, toolchain, or CI/CD updates cut from `develop`.

### 2. Mandatory Agent Workflow
1. **Always base work on up-to-date `develop`**:
   ```bash
   git checkout develop
   git pull origin develop
   git checkout -b feat/<feature-name>
   ```
2. **Never commit directly to `main`** or leave feature work unmerged on detached/ad-hoc branches.
3. **Execute Formatting, Linting & Verification Gate before committing/merging**:
   ```bash
   make format-check   # fourmolu -m check across src/ and test/
   make lint           # hlint across src/ and test/
   cabal check
   ./.agents/skills/formal-spec-verifier/scripts/verify_gate.sh --haskell
   ```
4. **Use Conventional Commits**:
   - `feat:` for new capabilities
   - `fix:` for bug fixes
   - `refactor:` for code restructuring without behavior change
   - `test:` for test additions or updates
   - `docs:` for documentation updates
   - `chore:` for build/dependency/tool updates

---

## Codebase Architecture

```
ollama-haskell.cabal           -- Cabal 3.0 specification & multi-GHC compatibility (GHC 9.4 - 9.12)
stack.yaml / stack-lts-*.yaml  -- Stack multi-resolver configurations
fourmolu.yaml                  -- Repository formatting rules (2-space indent, 100 cols, leading commas)
makefile                       -- lint, format, format-check, test, clean

src/
  Ollama.hs                    -- Canonical umbrella module re-exporting core API surface
  Ollama/
    Client.hs                  -- OllamaClient handle, newClient, withClient
    Client/
      Config.hs                -- OllamaClientConfig, retry policies, timeouts, auth
      Internal.hs              -- HTTP request dispatch, NDJSON conduit streaming, StreamLine error parsing
    API/
      Chat.hs                  -- ChatRequest/Response, chat, chatStructured, chatStream, accumulators
      Generate.hs              -- GenerateRequest/Response, generate, generateStructured, stream, accumulators
      Embed.hs                 -- EmbedRequest/Response, embed, embeddings
      Blobs.hs                 -- Blob management (createBlob, checkBlob)
      Models.hs                -- listModels, showModel, copyModel, deleteModel, preloadModel, unloadModel
      Models/Create.hs         -- createModel, createModelStream
      Models/Pull.hs           -- pullModel, pullStream, pullWith
      Models/Push.hs           -- pushModel, pushStream, pushWith
      Ps.hs                    -- ps, isModelRunning, getModelVramUsage
      Version.hs               -- getVersion
      SystemOne.hs             -- Ollama fast decision evaluation (/v1/systemone)
      WebSearch.hs             -- Ollama Cloud live web search & fetch grounding (/api/web_search, /api/web_fetch)
    Conversation.hs            -- STM-backed InMemoryStore, ConversationStore, chatWithTools loop
    Error.hs                   -- Typed OllamaError (HttpError, ApiError, DecodeError, StreamError, etc.)
    Streaming.hs               -- HasDone typeclass, accumulateStream combinators
    Testing.hs                 -- Deterministic mock clients (newMockClient, newMockClientSequence)
    MCP.hs                     -- Model Context Protocol bridge (mcp-server >= 0.3)
    Types/
      Common.hs                -- ModelName, Duration, Think, Logprob, Base64Image
      Format.hs                -- Format, SchemaFormat, JsonFormat
      Format/
        SchemaBuilder.hs       -- Type-safe JSON Schema builder DSL
        SchemaDerive.hs        -- Generic ToSchema / ToJsonType derivation
      Message.hs               -- Message, Role, ContentBlock, toolResultMessageWithId
      Model.hs                 -- ModelInfo, ModelDetails, RunningModel
      Options.hs               -- ModelOptions parameter tuning
      Tool.hs                  -- Tool, ToolCall, FunctionDef, parameter DSL
examples/                      -- Comprehensive runnable executables verifying each capability
test/                          -- Golden, Unit, and QuickCheck property test suites (Tasty)
```

---

## Core Capability Patterns

### `OllamaClient` Resource Pattern

```haskell
-- With default localhost:11434 configuration
client <- newClient

-- Scoped resource with automatic cleanup
withClient defaultClientConfig $ \client -> do
  res <- chat client req
  ...
```

### Type-Safe Structured Outputs Pattern

```haskell
data Extraction = Extraction
  { title :: !Text
  , tags :: ![Text]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

res <- chatStructured @Extraction client (chatRequest "qwen3.5:2b" msgs)
case res of
  Left err -> handleError err
  Right (StructuredResponse extracted raw) -> useData extracted
```

---

## Code Quality & Formatting Rules

1. **Compiler strictness**: All code must compile cleanly with `-Wall -Werror` / `--pedantic`. Avoid partial functions (`head`, `tail`, `fromJust`, `error`).
2. **Fourmolu formatting**: Always run `make format` or `fourmolu -m check` matching `fourmolu.yaml` (2-space indent, 100-column limit, leading commas and leading import/export lists).
3. **HLint cleanliness**: Always run `make lint` and address all HLint hints before committing.
4. **Cabal & Stack parity**: Ensure `ollama-haskell.cabal` and Stack configs remain in lockstep. Always run `cabal check` to ensure zero warnings.
5. **Preserve public API backwards compatibility**: Keep existing exported functions and data constructors compatible unless a breaking change is explicitly approved for a minor/major bump.

---

## Testing Rules

1. **Pure invariants**: Add `QuickCheck` property tests for pure transformations, parsers, accumulators, schema generation, and algebraic laws.
2. **IO & Provider boundaries**: Write unit tests using mock clients (`newMockClient`, `newMockClientSequence`) — **never require live network API keys in unit/CI test suites**.
3. **Streaming tests**: Verify incremental chunk emission and clean cancellation/error propagation, not just final aggregated output.
4. **Verify exact build & test commands**:
   - `stack test --pedantic`
   - `./.agents/skills/formal-spec-verifier/scripts/verify_gate.sh --haskell`
   - `make format-check && make lint`
   - `cabal check`

