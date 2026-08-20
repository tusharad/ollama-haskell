---
title: "Welcome to ollama-haskell"
category: "Overview"
description: "Industry-grade, feature-complete, type-safe Haskell client library for the Ollama local LLM engine."
---

::: {.callout .callout-tip}
<div class="callout-title">⚡ High-Performance Local AI for Haskell</div>
`ollama-haskell` brings production-ready type-safety, deterministic JSON outputs, composable streaming, and Model Context Protocol (MCP) tooling to locally hosted Large Language Models.
:::

## ⚡ 5-Line Quick Start {#quick-start}

Interact with local models with zero boilerplate:

```haskell
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text.IO qualified as TIO
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  res    <- chat client $ chatRequest "qwen3.5:2b" (userMessage "Why is the sky blue?" :| [])
  case res of
    Left err   -> print err
    Right resp -> mapM_ (TIO.putStrLn . messageContent) (crMessage resp)
```

## 🌟 Core Capabilities {#core-capabilities}

<div class="feature-grid">
    <div class="feature-card">
        <div class="card-icon">🌊</div>
        <h3>First-Class Conduit Streaming</h3>
        <p>Stream tokens asynchronously with minimal memory footprint and zero buffering delays via <code>chatStream</code> and <code>generateStream</code>.</p>
    </div>

    <div class="feature-card">
        <div class="card-icon">📐</div>
        <h3>Generic JSON Schema Derivation</h3>
        <p>Derive strict JSON schemas directly from Haskell records via <code>GHC.Generics</code> with <code>ToSchema</code> and <code>formatFor</code>.</p>
    </div>

    <div class="feature-card">
        <div class="card-icon">🔌</div>
        <h3>Model Context Protocol (MCP)</h3>
        <p>Seamless bidirectional bridging between Ollama tool calls and MCP servers via <code>mcp-server</code> over stdio and HTTP.</p>
    </div>

    <div class="feature-card">
        <div class="card-icon">🧠</div>
        <h3>Thinking / Reasoning Models</h3>
        <p>Native ADT support for deep reasoning models like <code>qwen3.5</code> and <code>deepseek-r1</code> with <code>ThinkingLevel</code> controls.</p>
    </div>

    <div class="feature-card">
        <div class="card-icon">💾</div>
        <h3>STM Conversation Store</h3>
        <p>Transactional, thread-safe in-memory session management with <code>InMemoryStore</code> and <code>ConversationStore</code> typeclasses.</p>
    </div>

    <div class="feature-card">
        <div class="card-icon">🧪</div>
        <h3>Built-in Mock Testing</h3>
        <p>Test your AI-enabled pipelines in CI/CD without running a live Ollama daemon using <code>Ollama.Testing</code>.</p>
    </div>
</div>

## 📦 Installation {#installation}

Add `ollama-haskell` to your project dependencies in `.cabal`:

```cabal
build-depends:
    base >= 4.17 && < 5
  , ollama-haskell >= 0.4.0.0
```

Or in Stack `package.yaml`:

```yaml
dependencies:
  - ollama-haskell >= 0.4.0.0
```

## 🧭 Next Steps {#next-steps}

<div class="feature-grid">
    <a href="/ollama-101.html" class="feature-card" style="text-decoration: none;">
        <div class="card-icon">🚀</div>
        <h3>Ollama 101 Beginner Guide</h3>
        <p>Never used Ollama before? Learn how to install Ollama, download models, and run your first query in under 2 minutes.</p>
    </a>
    <a href="/tutorials/chat.html" class="feature-card" style="text-decoration: none;">
        <div class="card-icon">📖</div>
        <h3>Feature Tutorials</h3>
        <p>Hands-on guides covering chat streaming, tool calling, MCP servers, embeddings, and structured outputs.</p>
    </a>
</div>
