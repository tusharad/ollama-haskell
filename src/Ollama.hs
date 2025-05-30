{-# LANGUAGE DuplicateRecordFields #-}

{- |
Module:      Data.Ollama
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Description: Ollama client for Haskell
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental

#Ollama-Haskell#
This library lets you run LlMs from within Haskell projects. Inspired by `ollama-python`.
-}
module Ollama
  ( -- * Main APIs

    -- ** Generate Texts
    generate
  , generateM
  , generateJson
  , generateJsonM
  , defaultGenerateOps
  , GenerateOps (..)
  , GenerateResponse (..)

    -- ** Chat with LLMs
  , chat
  , chatM
  , chatJson
  , chatJsonM
  , Role (..)
  , defaultChatOps
  , ChatResponse (..)
  , ChatOps (..)
  , InputTool (..)
  , Function (..)
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
  , setNContext
  , insert
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
  )
where

import Data.Ollama.Chat
import Data.Ollama.Common.Config
import Data.Ollama.Common.Error
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
