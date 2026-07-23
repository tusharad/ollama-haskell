{- |
Module      : Ollama
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Top-level umbrella re-export module for the Ollama Haskell client library.

== Quick Example

@
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  let req = chatRequest "llama3.2" (userMessage "Why is the sky blue?" :| [])
  result <- chat client req
  case result of
    Left err -> print err
    Right resp -> case crMessage resp of
      Just msg -> putStrLn (messageContent msg)
      Nothing  -> putStrLn "No message returned"
@

@since 1.0.0.0
-}
module Ollama (
  -- * Client
  OllamaClient,
  newClient,
  defaultClient,
  clientFromEnv,
  closeClient,
  withClient,

  -- * Config & Retry
  OllamaClientConfig (..),
  defaultConfig,
  RetryPolicy (..),
  LogLevel (..),

  -- * API Endpoints

  -- ** Chat
  chat,
  chatStream,
  ChatRequest (..),
  ChatResponse (..),
  chatRequest,

  -- ** Generate
  generate,
  generateStream,
  GenerateRequest (..),
  GenerateResponse (..),
  generateRequest,

  -- ** Embeddings
  embed,
  EmbedRequest (..),
  EmbedResponse (..),
  embedRequest,

  -- ** Model Management
  listModels,
  showModel,
  copyModel,
  deleteModel,
  ListResponse (..),
  ShowResponse (..),

  -- ** Pull & Push
  pull,
  pullStream,
  push,
  pushStream,
  PullResponse (..),
  PushResponse (..),

  -- ** Blobs
  checkBlob,
  pushBlob,

  -- ** System
  getVersion,
  listRunning,
  RunningModelsResponse (..),

  -- * Types & Primitives
  ModelName (..),
  mkModelName,
  Digest (..),
  Base64Image (..),
  Duration (..),
  durationToSeconds,
  durationToMillis,
  Version (..),
  Think (..),
  ThinkingLevel (..),

  -- ** Messages
  Role (..),
  Message (..),
  userMessage,
  systemMessage,
  assistantMessage,
  toolMessage,
  toolResultMessage,
  imageMessage,

  -- ** Tools & Functions
  Tool (..),
  FunctionDef (..),
  FunctionParameters (..),
  ToolCall (..),
  ToolCallFunction (..),

  -- ** Options & Format
  ModelOptions (..),
  defaultOptions,
  Format (..),

  -- * Error Handling
  OllamaError (..),
  isRetryable,
  throwOllama,

  -- * Streaming
  HasDone (..),
) where

import Ollama.API.Blobs
import Ollama.API.Chat
import Ollama.API.Embed
import Ollama.API.Generate
import Ollama.API.Models
import Ollama.API.Models.Pull
import Ollama.API.Models.Push
import Ollama.API.Ps
import Ollama.API.Version
import Ollama.Client
import Ollama.Client.Config
import Ollama.Error
import Ollama.Streaming
import Ollama.Types
