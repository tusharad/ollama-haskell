{-# LANGUAGE DuplicateRecordFields #-}
module Ollama
  ( list,
    Models(..),
    ModelInfo(..),
    chat,
    chatOps,
    Message (..),
    generate,
    showModel,
    createModel,
    createModelOps,
    copyModel,
    pull,
    pullOps,
    push,
    embedding,
    embeddingOps,
    ps,
    RunningModels(..),
    RunningModel(..),
    deleteModel,
    chatReturning,
    chatOpsReturning,
    ChatResponse(..),
    pushOps,
    showModelOps,
    ShowModelResponse(..)
  )
where

import Data.Ollama.Chat (Message (..), chat,chatOps,ChatResponse(..),chatReturning,chatOpsReturning)
import Data.Ollama.Copy (copyModel)
import Data.Ollama.Create (createModel,createModelOps)
import Data.Ollama.Delete (deleteModel)
import Data.Ollama.Embeddings (embedding,embeddingOps)
import Data.Ollama.Generate (generate)
import Data.Ollama.List (list,Models(..),ModelInfo (..))
import Data.Ollama.Ps (ps,RunningModels(..),RunningModel(..))
import Data.Ollama.Pull (pull,pullOps)
import Data.Ollama.Push (push,pushOps)
import Data.Ollama.Show (showModel,showModelOps,ShowModelResponse(..))
