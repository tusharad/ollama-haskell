{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Benchmark suite for Ollama client JSON serialization and URL normalization operations.

@since 1.0.0.0
-}
module Main (main) where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Ollama.API.Chat (ChatRequest, ChatResponse, chatRequest)
import Ollama.API.Embed (EmbedRequest, EmbedResponse, embedRequest)
import Ollama.API.Generate (GenerateRequest, GenerateResponse, generateRequest)
import Ollama.API.Models.Create (CreateRequest, defaultCreateRequest)
import Ollama.Testing (mockChatResponse, mockEmbedResponse, mockGenerateResponse)
import Ollama.Types.Common (ModelName (..))
import Ollama.Types.Message (userMessage)
import Test.Tasty.Bench (bench, bgroup, defaultMain, whnf)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "JSON Encoding"
        [ bench "GenerateRequest" $ whnf encode sampleGenerateRequest
        , bench "ChatRequest" $ whnf encode sampleChatRequest
        , bench "EmbedRequest" $ whnf encode sampleEmbedRequest
        , bench "CreateRequest" $ whnf encode sampleCreateRequest
        ]
    , bgroup
        "JSON Decoding"
        [ bench "GenerateResponse" $ whnf (decode @GenerateResponse) sampleGenerateBytes
        , bench "ChatResponse" $ whnf (decode @ChatResponse) sampleChatBytes
        , bench "EmbedResponse" $ whnf (decode @EmbedResponse) sampleEmbedBytes
        ]
    ]

sampleGenerateRequest :: GenerateRequest
sampleGenerateRequest = generateRequest (ModelName "llama3.2") "Why is the sky blue?"

sampleChatRequest :: ChatRequest
sampleChatRequest = chatRequest (ModelName "llama3.2") (userMessage "Hello, world!" :| [])

sampleEmbedRequest :: EmbedRequest
sampleEmbedRequest = embedRequest (ModelName "nomic-embed-text") ["Hello", "World", "Vector"]

sampleCreateRequest :: CreateRequest
sampleCreateRequest = defaultCreateRequest (ModelName "custom-llama")

sampleGenerateBytes :: ByteString
sampleGenerateBytes = encode $ mockGenerateResponse (ModelName "llama3.2") "Sky is blue"

sampleChatBytes :: ByteString
sampleChatBytes = encode $ mockChatResponse (ModelName "llama3.2") "Hello there"

sampleEmbedBytes :: ByteString
sampleEmbedBytes = encode $ mockEmbedResponse (ModelName "nomic") [[0.1, 0.2]]
