{-# LANGUAGE DuplicateRecordFields #-}
{- | 
  #Ollama-Haskell#
  This library lets you run LlMs from within Haskell projects. Inspired by `ollama-python`.
-}
module Ollama
  ( -- * Main APIs
    -- ** Generate Texts
    generate,
    generateOps,
    generateReturningResponse,
    generateOpsReturningResponse,
    generateReturningResponse',
    generateOpsReturningResponse',
    -- ** Chat with LLMs
    chat,
    chatOps,
    chatReturning,
    chatOpsReturning,
    chatReturning',
    chatOpsReturning',
    -- ** Embeddings
    embedding,
    embeddingOps,
    -- * Ollama operations
    -- ** Copy Models
    copyModel,
    -- ** Create Models
    createModel,
    createModelOps,
    -- ** Delete Models
    deleteModel,
    -- ** List Models
    list,
    -- ** List currently running models
    ps,
    -- ** Push and Pull
    push,
    pushOps,
    pull,
    pullOps,
    -- ** Show Model Info
    showModel,
    showModelOps,
    -- * Types
    ShowModelResponse(..),
    Models(..),
    ModelInfo(..),
    RunningModels(..),
    RunningModel(..),
    ChatResponse(..),
    Message(..)
  )
where


import Data.Ollama.Generate (generate, generateOps,generateReturningResponse,generateOpsReturningResponse,generateReturningResponse', generateOpsReturningResponse')
import Data.Ollama.Chat (Message (..), chat,chatOps,ChatResponse(..),chatReturning,chatOpsReturning,chatReturning',chatOpsReturning')
import Data.Ollama.Copy (copyModel)
import Data.Ollama.Create (createModel,createModelOps)
import Data.Ollama.Delete (deleteModel)
import Data.Ollama.Embeddings (embedding,embeddingOps)
import Data.Ollama.List (list,Models(..),ModelInfo (..))
import Data.Ollama.Ps (ps,RunningModels(..),RunningModel(..))
import Data.Ollama.Pull (pull,pullOps)
import Data.Ollama.Push (push,pushOps)
import Data.Ollama.Show (showModel,showModelOps,ShowModelResponse(..))
