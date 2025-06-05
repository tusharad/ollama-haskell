{-# LANGUAGE DuplicateRecordFields #-}

{- |
Module      : Data.Ollama
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Portability : portable

== Ollama Haskell

This module provides a high-level Haskell interface to the [Ollama](https://ollama.com) API 
    for interacting with local LLMs. It includes support for:

- Text generation (sync/streaming)
- Conversational chat (with tools and images)
- Embeddings
- Model management (pull, push, delete, list, show)
- Structured outputs
- Custom configuration and model options

Inspired by @ollama-python@, this library is built to offer idiomatic Haskell bindings 
over Ollama’s HTTP API.

== 🔧 Usage

Import this module as a top-level interface:

@
import Ollama
@

All functions return @Either OllamaError a@ or can be used in a Monad stack using 
their @\*M@ variants.

== 🔑 Main APIs

=== ✍️ Generate Text

- 'generate', 'generateM' – Generate text from a model
- 'defaultGenerateOps' – Default generation parameters
- 'GenerateOps', 'GenerateResponse' – Request and response types

=== 💬 Chat with LLMs

- 'chat', 'chatM' – Send chat messages to a model
- 'ChatOps', 'ChatResponse', 'Role', 'Message' – Chat input/output types
- Supports tools via 'InputTool', 'FunctionDef', 'OutputFunction', etc.

=== 🧠 Embeddings

- 'embedding', 'embeddingM' – Generate vector embeddings
- 'EmbeddingOps', 'EmbeddingResp' – Request/response types

=== 📦 Model Management

- 'copyModel', 'createModel', 'deleteModel'
- 'list' – List all installed models
- 'ps', 'psM' – Show running models
- 'showModel', 'showModelM' – Show model info
- 'pull', 'push' – Pull/push models (with progress support)

=== ⚙️ Configuration

- 'defaultOllamaConfig' – Modify host, retries, streaming, etc.
- 'withOnModelStart', 'withOnModelFinish', 'withOnModelError' – Hook support

=== 🧰 Utilities

- 'defaultModelOptions', 'encodeImage', 'withOllamaRequest'
- 'loadGenModel', 'unloadGenModel' – Load/unload generation models
- 'getVersion' – Ollama server version

== 🧾 Types

All request/response payloads and enums are exposed, including:

- 'ModelOptions', 'OllamaConfig', 'OllamaError', 'Format'
- 'Models', 'ModelInfo', 'ModelDetails', 'ShowModelResponse'
- 'RunningModels', 'RunningModel', 'Version'
-}
module Ollama
  ( -- * Main APIs

    -- ** Generate Texts
    generate
  , generateM
  , defaultGenerateOps
  , GenerateOps (..)
  , GenerateResponse (..)

    -- ** Chat with LLMs
  , chat
  , chatM
  , Role (..)
  , defaultChatOps
  , ChatResponse (..)
  , ChatOps (..)
  , InputTool (..)
  , FunctionDef (..)
  , FunctionParameters (..)
  , ToolCall (..)
  , OutputFunction (..)

    -- ** Embeddings
  , embedding
  , embeddingOps
  , embeddingM
  , embeddingOpsM
  , EmbeddingOps (..)
  , EmbeddingResp (..)

    -- ** Copy Models
  , copyModel
  , copyModelM

    -- ** Create Models
  , createModel
  , createModelM

    -- ** Delete Models
  , deleteModel
  , deleteModelM

    -- ** List Models
  , list

    -- ** List currently running models
  , ps
  , psM

    -- ** Push and Pull
  , push
  , pushM
  , pull
  , pullM
  , pullOps
  , pullOpsM

    -- ** Show Model Info
  , showModel
  , showModelOps
  , showModelM
  , showModelOpsM

    -- * Ollama config
  , defaultOllamaConfig
  , withOnModelStart
  , withOnModelFinish
  , withOnModelError

    -- * Utils
  , defaultModelOptions
  , ModelOptions (..)
  , encodeImage
  , withOllamaRequest
  , getVersion
  , loadGenModel
  , unloadGenModel
  , loadGenModelM
  , unloadGenModelM

    -- * Types
  , ShowModelResponse (..)
  , Models (..)
  , ModelInfo (..)
  , ModelDetails (..)
  , ShowModelInfo (..)
  , RunningModels (..)
  , RunningModel (..)
  , Message (..)
  , Format (..)
  , OllamaError (..)
  , OllamaConfig (..)
  , Version (..)
  )
where

import Data.Ollama.Chat
import Data.Ollama.Common.Config
import Data.Ollama.Common.Types
import Data.Ollama.Common.Utils
import Data.Ollama.Copy
import Data.Ollama.Create
import Data.Ollama.Delete
import Data.Ollama.Embeddings
import Data.Ollama.Generate
import Data.Ollama.List
import Data.Ollama.Load
import Data.Ollama.Ps
import Data.Ollama.Pull
import Data.Ollama.Push
import Data.Ollama.Show
