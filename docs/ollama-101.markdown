---
title: "Ollama 101: The Beginner's Complete Guide"
category: "Getting Started"
description: "Everything you need to know to run local AI models on your machine and connect them with Haskell."
---

## What is Ollama?

**[Ollama](https://ollama.com)** is an open-source, high-performance runtime that packages model weights, configurations, and GPU acceleration into a unified daemon and HTTP API. It lets you run open-weights LLMs (such as Llama 3, Qwen 2.5/3.5, Mistral, Gemma 2/3, and DeepSeek) entirely on your local hardware — privately, with zero subscription fees and zero data leaving your machine.

With `ollama-haskell`, you interact with Ollama's local HTTP REST API (`http://127.0.0.1:11434`) using a type-safe, ergonomic Haskell interface.

---

## Step 1: Install Ollama

Installing Ollama takes one command or installer:

### macOS
Download the installer from [ollama.com/download/mac](https://ollama.com/download) or use Homebrew:
```bash
brew install ollama
```

### Linux
Run the official installation script:
```bash
curl -fsSL https://ollama.com/install.sh | sh
```

### Windows
Download the installer from [ollama.com/download/windows](https://ollama.com/download).

---

## Step 2: Start the Ollama Daemon

Ollama runs as a background service listening on `http://127.0.0.1:11434`.

- On macOS and Windows, starting the Ollama desktop app runs the daemon automatically.
- On Linux or from a terminal:
```bash
ollama serve
```

To verify the server is running:
```bash
curl http://127.0.0.1:11434/api/version
# Output: {"version":"0.5.x"}
```

---

## Step 3: Your First Haskell Program with Ollama

Create a new Haskell file or run in GHCi:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text.IO qualified as TIO
import Ollama

main :: IO ()
main = do
  -- 1. Create client pointing to http://127.0.0.1:11434
  client <- defaultClient

  -- 2. Build a chat request
  let messages = userMessage "Explain quantum computing in one sentence." :| []
      req = chatRequest "qwen3.5:2b" messages

  -- 3. Execute chat call
  putStrLn "Sending prompt to local LLM..."
  res <- chat client req

  -- 4. Handle response
  case res of
    Left err   -> putStrLn $ "Error: " <> show err
    Right resp -> case crMessage resp of
      Just msg -> TIO.putStrLn $ "LLM Response:\n" <> messageContent msg
      Nothing  -> putStrLn "No message returned"
```

---

## Step 4: Essential Ollama CLI Commands

Keep these CLI commands handy while developing:

```bash
# List all locally downloaded models
ollama list

# Check currently running/loaded models in VRAM
ollama ps

# Test a model directly in terminal
ollama run qwen3.5:2b "Hello! Introduce yourself."

# Delete a model to free disk space
ollama rm llama3.2:3b
```

---

## Step 6: Next Steps

Now that your local Ollama environment is running, check out the core tutorials:

- [💬 Chat & Conduit Streaming](/tutorials/chat.html) — Stream responses token-by-token.
- [📐 Generic Schema Derivation](/tutorials/structured-outputs.html) — Parse guaranteed type-safe JSON.
- [🔌 Model Context Protocol (MCP)](/tutorials/mcp.html) — Connect LLMs to MCP tool servers.
