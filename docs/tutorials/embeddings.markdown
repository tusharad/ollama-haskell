---
title: Embeddings & Vector Search
category: Feature Tutorials
description: Generate dense vector embeddings for semantic search, document clustering, and RAG pipelines.
---

## Overview

Vector embeddings convert text into numerical arrays (vectors) that capture semantic meaning. Texts with similar meanings produce vectors that are close together in multi-dimensional space.

Ollama provides high-speed local embedding models such as `nomic-embed-text` and `all-minilm`.

---

## 1. Generating Embeddings

Use `embed` with `embedRequest`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Ollama

main :: IO ()
main = do
  client <- defaultClient

  -- Generate embedding for a single text
  let req = embedRequest "nomic-embed-text" ["Haskell is a purely functional programming language."]
  res <- embed client req

  case res of
    Left err   -> print err
    Right resp -> do
      let vectors = erEmbeddings resp
      putStrLn $ "Generated " <> show (length vectors) <> " vector(s)"
      case vectors of
        (vec : _) -> do
          putStrLn $ "Vector dimensions: " <> show (length vec)
          putStrLn $ "First 5 dimensions: " <> show (take 5 vec)
        [] -> putStrLn "No embeddings returned"
```

---

## 2. Batch Embedding & Semantic Similarity Search

Generate embeddings for multiple documents in a single request and rank them using cosine similarity:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (sortBy)
import Data.Ord (Down (..))
import Data.Text (Text)
import Ollama

-- Calculate cosine similarity between two vectors
cosineSimilarity :: [Double] -> [Double] -> Double
cosineSimilarity u v =
  let dotProduct = sum $ zipWith (*) u v
      normA = sqrt . sum $ map (^ (2 :: Int)) u
      normB = sqrt . sum $ map (^ (2 :: Int)) v
   in if normA == 0 || normB == 0 then 0 else dotProduct / (normA * normB)

documents :: [Text]
documents =
  [ "GHC compiles Haskell source code to native machine instructions."
  , "Bananas and apples are nutritious fruits."
  , "Type inference with Hindley-Milner algorithms guarantees static safety."
  , "The weather forecast calls for rain tomorrow."
  ]

main :: IO ()
main = do
  client <- defaultClient

  -- 1. Embed documents
  docRes <- embed client $ embedRequest "nomic-embed-text" documents

  -- 2. Embed user search query
  queryRes <- embed client $ embedRequest "nomic-embed-text" ["How does Haskell typing work?"]

  case (docRes, queryRes) of
    (Right dResp, Right qResp) -> do
      case (erEmbeddings dResp, erEmbeddings qResp) of
        (docVecs, queryVec : _) -> do
          let scored = zip documents (map (cosineSimilarity queryVec) docVecs)
              ranked = sortBy (\(_, s1) (_, s2) -> compare (Down s1) (Down s2)) scored

          putStrLn "--- Top Search Results ---"
          mapM_ (\(doc, score) -> putStrLn $ "[" <> show (round (score * 100) :: Int) <> "% match] " <> show doc) ranked
        _ -> putStrLn "Missing embeddings"
    _ -> putStrLn "Error computing embeddings"
```
