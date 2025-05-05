{-# LANGUAGE DuplicateRecordFields #-}
{- |
  #Ollama-Haskell#
  This library lets you run LlMs from within Haskell projects. Inspired by `ollama-python`.
-}
module Ollama
  ( -- * Main APIs

    -- ** Generate Texts
    generate
  , generateJson
  , defaultGenerateOps
  , GenerateOps (..)
  , GenerateResponse (..)

    -- ** Chat with LLMs
  , chat
  , chatJson
  , Role (..)
  , defaultChatOps
  , ChatResponse (..)
  , ChatOps (..)

    -- ** Embeddings
  , embedding
  , embeddingOps
  , EmbeddingOps (..)
  , EmbeddingResp (..)

    -- * Ollama operations

    -- ** Copy Models
  , copyModel
  , copyModelOps

    -- ** Create Models
  , createModel
  , createModelOps

    -- ** Delete Models
  , deleteModel
  , deleteModelOps

    -- ** List Models
  , list
  , listOps

    -- ** List currently running models
  , ps
  , psOps

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
  , ModelDetails (..)
  , ShowModelInfo (..)
  , RunningModels (..)
  , RunningModel (..)
  , Message (..)
  , Format(..)
  )
where

import Data.Ollama.Chat
  ( ChatOps (..)
  , ChatResponse (..)
  , Message (..)
  , Role (..)
  , chat
  , chatJson
  , defaultChatOps
  )
import Data.Ollama.Copy (copyModel, copyModelOps)
import Data.Ollama.Create (createModel, createModelOps)
import Data.Ollama.Delete (deleteModel, deleteModelOps)
import Data.Ollama.Embeddings (embedding, embeddingOps, EmbeddingOps (..), EmbeddingResp (..))
import Data.Ollama.Generate
  ( GenerateOps (..)
  , GenerateResponse (..)
  , defaultGenerateOps
  , generate
  , generateJson
  )
import Data.Ollama.List (ModelInfo (..), Models (..), list, listOps)
import Data.Ollama.Ps (RunningModel (..), RunningModels (..), ps, psOps)
import Data.Ollama.Pull (pull, pullOps)
import Data.Ollama.Push (push, pushOps)
import Data.Ollama.Show (ShowModelResponse (..), ShowModelInfo (..), showModel, showModelOps)
import Data.Ollama.Common.Types (Format (..), ModelDetails (..))
