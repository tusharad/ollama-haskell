{-# OPTIONS_GHC -Wno-deprecations #-}

{- |
Module      : Test.Ollama.Unit.Testing
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Unit tests for Ollama.Testing mock client and helper functions.

@since 1.0.0.0
-}
module Test.Ollama.Unit.Testing (testingTests) where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Ollama.API.Chat (ChatResponse (..), chat, chatRequest)
import Ollama.API.Embed (
  EmbedResponse (..),
  EmbeddingsRequest (..),
  EmbeddingsResponse (..),
  embed,
  embedRequest,
  embeddings,
 )
import Ollama.API.Generate (GenerateResponse (..), generate, generateRequest)
import Ollama.API.Models (ListResponse (..), listModels)
import Ollama.Testing (
  mockChatResponse,
  mockEmbedResponse,
  mockGenerateResponse,
  mockListModelsResponse,
  withMockClient,
 )
import Ollama.Types.Common (ModelName (..))
import Ollama.Types.Message (userMessage)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

testingTests :: TestTree
testingTests =
  testGroup
    "Unit Testing Infrastructure Tests"
    [ testCase "Mock generate response" $ do
        let respPayload = mockGenerateResponse (ModelName "llama3.2") "Sky is blue"
            encoded = encode respPayload
        withMockClient (LBS.toStrict encoded) $ \client -> do
          res <- generate client (generateRequest (ModelName "llama3.2") "Why?")
          case res of
            Left err -> fail $ "Expected success, got: " <> show err
            Right resp -> assertEqual "Matching text" "Sky is blue" (grResponse resp)
    , testCase "Mock chat response" $ do
        let respPayload = mockChatResponse (ModelName "llama3.2") "Hello there"
            encoded = encode respPayload
        withMockClient (LBS.toStrict encoded) $ \client -> do
          res <- chat client (chatRequest (ModelName "llama3.2") (userMessage "Hi" :| []))
          case res of
            Left err -> fail $ "Expected success, got: " <> show err
            Right resp -> assertEqual "Matching model" (ModelName "llama3.2") (crModel resp)
    , testCase "Mock embed response" $ do
        let respPayload = mockEmbedResponse (ModelName "nomic") [[0.1, 0.2]]
            encoded = encode respPayload
        withMockClient (LBS.toStrict encoded) $ \client -> do
          res <- embed client (embedRequest (ModelName "nomic") ["test"])
          case res of
            Left err -> fail $ "Expected success, got: " <> show err
            Right resp -> assertEqual "Matching embeddings" [[0.1, 0.2]] (erEmbeddings resp)
    , testCase "Mock deprecated embeddings endpoint" $ do
        let respPayload = EmbeddingsResponse [0.5, 0.6]
            encoded = encode respPayload
        withMockClient (LBS.toStrict encoded) $ \client -> do
          res <- embeddings client (EmbeddingsRequest (ModelName "nomic") "test" Nothing Nothing)
          case res of
            Left err -> fail $ "Expected success, got: " <> show err
            Right resp -> assertEqual "Matching embedding" [0.5, 0.6] (ebrEmbedding resp)
    , testCase "Mock list models response" $ do
        let respPayload = mockListModelsResponse [ModelName "llama3.2", ModelName "nomic"]
            encoded = encode respPayload
        withMockClient (LBS.toStrict encoded) $ \client -> do
          res <- listModels client
          case res of
            Left err -> fail $ "Expected success, got: " <> show err
            Right resp -> assertEqual "Matching count" 2 (length (models resp))
    ]
