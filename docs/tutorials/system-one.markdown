---
title: System One Decision Evaluation
category: Feature Tutorials
description: High-speed classification, binary probability, and scoring using specialized decision models via /v1/systemone.
---

## What is System One?

Ollama's System One API (`/v1/systemone`) provides dedicated endpoints for high-speed, structured decision-making models (e.g. `nimble`). Instead of asking a general-purpose chat model to produce free-form text, System One allows you to supply state context and simultaneously evaluate multiple questions with calibrated confidence scores and probabilities.

---

## Question Types

`ollama-haskell` models System One questions through the `SystemOneQuestion` ADT:

1. **Choice Questions (`choiceQuestion`)**: Categorical evaluation over multiple discrete options with optional per-option criteria descriptions.
2. **Noul Questions (`noulQuestion` / `noulQuestionWithCriteria`)**: Binary true/false probability estimation.
3. **Score Questions (`scoreQuestion`)**: Ordinal scale rating (e.g., 1 to 5) with optional legend mappings.

---

## Example Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ollama

main :: IO ()
main = do
  client <- defaultClient

  let state = "The customer requested a full refund after 45 days, but policy limit is 30 days."

      q1 = choiceQuestion
        "Is the customer eligible for an automatic or discretionary refund?"
        (Map.fromList
          [ ("eligible", Just "Within standard 30 day window")
          , ("discretionary", Just "Requires manager escalation")
          , ("ineligible", Just "Strict policy violation")
          ])

      q2 = noulQuestion "Will this request result in customer churn if denied?"

      q3 = scoreQuestion
        "Rate customer frustration risk from 1 (low) to 5 (extreme)."
        ["1", "2", "3", "4", "5"]

      questions = Map.fromList
        [ ("eligibility", q1)
        , ("churn_risk", q2)
        , ("frustration_score", q3)
        ]

      req = systemOneRequest "nimble" state questions

  res <- systemOne client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> do
      mapM_ (\(name, ans) -> case ans of
        AnswerChoice ca -> do
          putStrLn $ "Question [" <> T.unpack name <> "]: " <> T.unpack (choiceResult ca)
          putStrLn $ "  Confidence: " <> show (choiceConfidence ca)
          putStrLn $ "  Probabilities: " <> show (choiceProbabilities ca)
        AnswerNoul p ->
          putStrLn $ "Question [" <> T.unpack name <> "]: Probability -> " <> show p
        AnswerScore sa -> do
          putStrLn $ "Question [" <> T.unpack name <> "]: Score -> " <> show (scoreValue sa)
          putStrLn $ "  Confidence: " <> show (scoreConfidence sa)
        ) (Map.toList (sorAnswers resp))
```
