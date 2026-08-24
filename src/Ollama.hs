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
  chatEvalTokensPerSecond,
  chatPromptEvalTokensPerSecond,

  -- ** Generate
  generate,
  generateStream,
  GenerateRequest (..),
  GenerateResponse (..),
  generateRequest,
  evalTokensPerSecond,
  promptEvalTokensPerSecond,

  -- ** Embeddings
  embed,
  EmbedRequest (..),
  EmbedResponse (..),
  embedRequest,
  embeddings,
  EmbeddingsRequest (..),
  EmbeddingsResponse (..),

  -- ** Model Management
  listModels,
  showModel,
  copyModel,
  deleteModel,
  ListResponse (..),
  ModelInfo (..),
  RunningModel (..),
  ShowResponse (..),

  -- ** Create
  createModel,
  createModelStream,
  defaultCreateRequest,
  CreateRequest (..),
  CreateResponse (..),
  QuantizationType (..),

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
  tokensPerSecond,
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
  ToSchema (..),
  ToJsonType (..),
  schemaFor,
  formatFor,

  -- ** Schema Builder DSL
  JsonType (..),
  Property (..),
  Schema (..),
  SchemaBuilder,
  emptyObject,
  addProperty,
  addObjectProperty,
  requireField,
  requireFields,
  buildSchema,
  objectOf,
  arrayOf,
  printSchema,
  (|+),
  (|++),
  (|!),
  (|!!),

  -- * Error Handling
  OllamaError (..),
  isRetryable,
  throwOllama,

  -- * Streaming
  HasDone (..),
  collectStream,
  foldStream,

  -- * Testing Infrastructure
  newMockClient,
  withMockClient,
  mockGenerateResponse,
  mockChatResponse,
  mockEmbedResponse,
  mockListModelsResponse,

  -- * Conversation Store
  Conversation (..),
  ConversationStore (..),
  InMemoryStore (..),
  initInMemoryStore,
  saveConversationInMemory,
  loadConversationInMemory,
  listConversationsInMemory,
  deleteConversationInMemory,

  -- * Model Context Protocol (MCP) Integration
  module Ollama.MCP,
) where

import Ollama.API.Blobs
import Ollama.API.Chat
import Ollama.API.Embed
import Ollama.API.Generate
import Ollama.API.Models
import Ollama.API.Models.Create
import Ollama.API.Models.Pull
import Ollama.API.Models.Push
import Ollama.API.Ps
import Ollama.API.Version
import Ollama.Client
import Ollama.Client.Config
import Ollama.Conversation
import Ollama.Error
import Ollama.MCP
import Ollama.Streaming
import Ollama.Testing
import Ollama.Types
