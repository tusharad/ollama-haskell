{- |
  #Ollama-Haskell#
  This library lets you run LlMs from within Haskell projects. Inspired by `ollama-python`.
-}
module Ollama
  ( -- * Main APIs

    -- ** Generate Texts
    generate
  , defaultGenerateOps
  , GenerateOps (..)
  , GenerateResponse (..)

    -- ** Chat with LLMs
  , chat
  , Role (..)
  , defaultChatOps
  , ChatResponse (..)
  , ChatOps (..)

    -- ** Embeddings
  , embedding
  , embeddingOps

    -- * Ollama operations

    -- ** Copy Models
  , copyModel

    -- ** Create Models
  , createModel
  , createModelOps

    -- ** Delete Models
  , deleteModel

    -- ** List Models
  , list

    -- ** List currently running models
  , ps

    -- ** Push and Pull
  , push
  , pushOps
  , pull
  , pullOps

    -- ** Show Model Info
  , showModel
  , showModelOps

    -- * Types
  , ShowModelResponse (..)
  , Models (..)
  , ModelInfo (..)
  , RunningModels (..)
  , RunningModel (..)
  , Message (..)
  )
where

import Data.Ollama.Chat
  ( ChatOps (..)
  , ChatResponse (..)
  , Message (..)
  , Role (..)
  , chat
  , defaultChatOps
  )
import Data.Ollama.Copy (copyModel)
import Data.Ollama.Create (createModel, createModelOps)
import Data.Ollama.Delete (deleteModel)
import Data.Ollama.Embeddings (embedding, embeddingOps)
import Data.Ollama.Generate
  ( GenerateOps (..)
  , GenerateResponse (..)
  , defaultGenerateOps
  , generate
  )
import Data.Ollama.List (ModelInfo (..), Models (..), list)
import Data.Ollama.Ps (RunningModel (..), RunningModels (..), ps)
import Data.Ollama.Pull (pull, pullOps)
import Data.Ollama.Push (push, pushOps)
import Data.Ollama.Show (ShowModelResponse (..), showModel, showModelOps)
