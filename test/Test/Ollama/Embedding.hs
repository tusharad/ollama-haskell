{-# LANGUAGE OverloadedStrings #-}

module Test.Ollama.Embedding (tests) where

import Data.Ollama.Embeddings
import Test.Tasty
import Test.Tasty.HUnit

-- Qwen doesn't support embeddings yet
-- TODO: Add embeddings supporting model.

-- testEmbeddingBasic :: TestTree
-- testEmbeddingBasic = testCase "Basic embedding with qwen3" $ do
--   res <- embedding "qwen3:0.6b" ["The sky is blue.", "Cats are independent."]
--   case res of
--     Left err -> assertFailure $ "Expected success, got error: " ++ show err
--     Right EmbeddingResp {..} -> do
--       assertEqual "Should return two embeddings" 2 (length respondedEmbeddings)
--       assertBool "Embeddings should not be empty" (not (any null respondedEmbeddings))

-- testEmbeddingWithOptions :: TestTree
-- testEmbeddingWithOptions = testCase "Embedding with truncate and keepAlive" $ do
--   let opts = defaultModelOptions {numKeep = Just 5, seed = Just 42}
--   res <-
--     embeddingOps
--       "qwen3.5:2b"
--       ["Hello world"]
--       (Just True)
--       (Just 30)
--       (Just opts)
--       Nothing
--       Nothing
--   case res of
--     Left err -> assertFailure $ "Unexpected error: " ++ show err
--     Right EmbeddingResp {..} -> do
--       assertEqual "Should return one embedding" 1 (length respondedEmbeddings)
--       assertBool
--         "Embedding vector should not be empty"
--         (not . null $ listToMaybe respondedEmbeddings)

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
    [ testEmbeddingInvalidModel
    ]
