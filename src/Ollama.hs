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
  , Format (..)
  )
where

import Data.Ollama.Chat
import Data.Ollama.Common.Types 
import Data.Ollama.Copy 
import Data.Ollama.Create 
import Data.Ollama.Delete 
import Data.Ollama.Embeddings 
import Data.Ollama.Generate
import Data.Ollama.List 
import Data.Ollama.Ps 
import Data.Ollama.Pull 
import Data.Ollama.Push 
import Data.Ollama.Show 
