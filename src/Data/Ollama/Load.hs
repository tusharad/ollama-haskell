{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Data.Ollama.Load
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Description: A high level module for load and unload model functions
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental
-}
module Data.Ollama.Load
  ( loadGenModel
  , unloadGenModel
  , loadChatModel
  , loadEmbedModel
  , unloadChatModel
  , unloadEmbedModel
  ) where

import Data.Ollama.Chat qualified as Chat
import Data.Ollama.Common.Error
import Data.Ollama.Common.Utils (commonNonStreamingHandler, withOllamaRequest)
import Data.Ollama.Embeddings qualified as Embed
import Data.Ollama.Generate qualified as Gen
import Data.Text (Text)

-- | load generating model in memory
loadGenModel :: Text -> IO (Either OllamaError ())
loadGenModel m = do
  let ops = Gen.defaultGenerateOps {Gen.modelName = m}
  withOllamaRequest "/api/generate" "POST" (Just ops) Nothing commonNonStreamingHandler

-- | unload generating model from memory
unloadGenModel :: Text -> IO (Either OllamaError ())
unloadGenModel m = do
  let ops = Gen.defaultGenerateOps {Gen.modelName = m, Gen.keepAlive = Just 0}
  withOllamaRequest "/api/generate" "POST" (Just ops) Nothing commonNonStreamingHandler

-- | load chat model in memory
loadChatModel :: Text -> IO (Either OllamaError ())
loadChatModel m = do
  let ops = Chat.defaultChatOps {Chat.chatModelName = m}
  withOllamaRequest "/api/chat" "POST" (Just ops) Nothing commonNonStreamingHandler

-- | unload chat model from memory
unloadChatModel :: Text -> IO (Either OllamaError ())
unloadChatModel m = do
  let ops = Chat.defaultChatOps {Chat.chatModelName = m, Chat.keepAlive = Just 0}
  withOllamaRequest "/api/chat" "POST" (Just ops) Nothing commonNonStreamingHandler

-- | load embedding model in memory
loadEmbedModel :: Text -> IO (Either OllamaError ())
loadEmbedModel m = do
  let ops = Embed.defaultEmbeddingOps {Embed.model = m}
  withOllamaRequest "/api/embed" "POST" (Just ops) Nothing commonNonStreamingHandler

-- | unload embedding model from memory
unloadEmbedModel :: Text -> IO (Either OllamaError ())
unloadEmbedModel m = do
  let ops = Embed.defaultEmbeddingOps {Embed.model = m, Embed.keepAlive = Just 0}
  withOllamaRequest "/api/embed" "POST" (Just ops) Nothing commonNonStreamingHandler
