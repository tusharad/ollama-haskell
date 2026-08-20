---
title: Model Context Protocol (MCP)
category: Feature Tutorials
description: Seamlessly bridge Ollama tool calling with Model Context Protocol (MCP) servers and clients in Haskell.
---

## What is Model Context Protocol (MCP)?

The **Model Context Protocol (MCP)** is an open standard that allows LLM applications to access external tools, prompts, and data resources across standardized transport layers (stdio, SSE, HTTP).

The `Ollama.MCP` module provides native bidirectional interoperability between Ollama tools and the official Hackage `mcp-server` library.

---

## 1. Bidirectional Type Conversions

`Ollama.MCP` provides pure conversion functions between Ollama and MCP data types:

| Conversion Function | Source Type | Target Type | Description |
| :--- | :--- | :--- | :--- |
| `toolToMcpDefinition` | Ollama `Tool` | MCP `ToolDefinition` | Converts an Ollama tool schema into an MCP server tool |
| `mcpDefinitionToTool` | MCP `ToolDefinition` | Ollama `Tool` | Converts an MCP tool definition for use in `chatTools` |
| `toolCallToMcpArgs` | Ollama `ToolCall` | `[(Text, Value)]` | Extracts argument key-value pairs from an Ollama tool call |
| `mcpContentToToolOutput`| MCP `Content` | `Text` | Extracts text from MCP tool execution content |

---

## 2. Using MCP Tools with Ollama

Fetch tool definitions from an MCP server and expose them directly to Ollama:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Ollama
import Ollama.MCP

-- Suppose you receive a ToolDefinition from an MCP client/server
externalMcpTool :: ToolDefinition
externalMcpTool = mkToolDefinition
  "search_knowledge_base"
  "Search internal markdown documents by semantic query."
  (describedSchema (Just "Query string") (SchemaString Nothing))

main :: IO ()
main = do
  client <- defaultClient

  -- 1. Convert MCP tool definition into an Ollama Tool
  let ollamaTool = mcpDefinitionToTool externalMcpTool

  -- 2. Query Ollama with the converted tool
  let req = (chatRequest "qwen3.5:2b" (userMessage "Find our vacation policy." :| []))
        { chatTools = Just [ollamaTool] }

  res <- chat client req
  case res of
    Right resp -> case crMessage resp of
      Just msg -> do
        case messageToolCalls msg of
          Just (call : _) -> do
            -- 3. Extract arguments ready for MCP execution
            let args = toolCallToMcpArgs call
            putStrLn $ "Arguments for MCP handler: " <> show args
          Nothing -> putStrLn "Direct response received"
      Nothing -> pure ()
    Left err -> print err
```

---

## 3. Running an MCP Server with Stdio or HTTP

You can host Haskell-native MCP servers directly using the re-exported MCP runtime functions:

```haskell
import Ollama.MCP

-- Run stdio MCP server for desktop agents (Claude Desktop, IDE sidecars)
runMyServer :: IO ()
runMyServer = runMcpServerStdio myServerInfo myHandlers
```
