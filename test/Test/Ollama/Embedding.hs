{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Ollama.Embedding (tests) where

import Data.Maybe (listToMaybe)
import Data.Ollama.Embeddings
import Test.Tasty
import Test.Tasty.HUnit

testEmbeddingBasic :: TestTree
testEmbeddingBasic = testCase "Basic embedding with qwen3" $ do
  res <- embedding "qwen3:0.6b" ["The sky is blue.", "Cats are independent."]
  case res of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right EmbeddingResp {..} -> do
      assertEqual "Should return two embeddings" 2 (length embedding_)
      assertBool "Embeddings should not be empty" (all (not . null) embedding_)

testEmbeddingWithOptions :: TestTree
testEmbeddingWithOptions = testCase "Embedding with truncate and keepAlive" $ do
  let opts = defaultModelOptions {numKeep = Just 5, seed = Just 42}
  res <- embeddingOps "qwen3:0.6b" ["Hello world"] (Just True) (Just 30) (Just opts) Nothing
  case res of
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right EmbeddingResp {..} -> do
      assertEqual "Should return one embedding" 1 (length embedding_)
      assertBool "Embedding vector should not be empty" (not . null $ listToMaybe embedding_)

testEmbeddingInvalidModel :: TestTree
testEmbeddingInvalidModel = testCase "Embedding with invalid model name" $ do
  res <- embedding "nonexistent-model" ["This should fail."]
  case res of
    Left _ -> return () -- Expected failure
    Right _ -> assertFailure "Expected failure with invalid model name"

tests :: TestTree
tests =
  testGroup
    "Embeddings tests"
    [ testEmbeddingBasic
    , testEmbeddingWithOptions
    , testEmbeddingInvalidModel
    ]
