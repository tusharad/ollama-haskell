---
title: Structured Outputs & JSON Schemas
category: Feature Tutorials
description: Enforce deterministic JSON output from LLMs with Generic ToSchema derivation and the SchemaBuilder DSL.
---

## Why Structured Outputs?

When LLMs generate unconstrained text, extracting data requires fragile regular expressions and unpredictable JSON parsing. With `ollama-haskell`, you can instruct the model to constrain its generation to a strict JSON Schema, guaranteeing that the model output decodes directly into your Haskell data structures.

---

## Method 1: Automatic Generic Derivation (`ToSchema`)

The easiest way to enforce structured outputs is by deriving `ToSchema` via `GHC.Generics`:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Ollama

-- 1. Define your Haskell record
data CityWeather = CityWeather
  { city        :: Text
  , temperature :: Double
  , conditions  :: Text
  , humidity    :: Maybe Int -- Optional field: omitted from 'required' array
  } deriving stock (Generic, Show)
    deriving anyclass (ToSchema, FromJSON)

main :: IO ()
main = do
  client <- defaultClient

  -- 2. Pass 'formatFor @CityWeather' in the chat request
  let prompt = userMessage "Give me the weather report for Tokyo right now." :| []
      req = (chatRequest "qwen3.5:2b" prompt)
        { chatFormat = Just (formatFor @CityWeather) }

  res <- chat client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> case crMessage resp of
      Just msg -> do
        let rawJson = messageContent msg
        putStrLn $ "Raw JSON from LLM: " <> show rawJson

        -- 3. Decode JSON directly into your strongly typed Haskell data
        case eitherDecode (BSL.fromStrict (TE.encodeUtf8 rawJson)) of
          Left decodeErr -> putStrLn $ "JSON Parse Error: " <> decodeErr
          Right weather  -> do
            putStrLn "Successfully decoded Haskell record:"
            print (weather :: CityWeather)
      Nothing -> putStrLn "No message returned"
```

---

## Supported Generic Schema Types

| Haskell Type | JSON Schema Representation | Note |
| :--- | :--- | :--- |
| `Text`, `String` | `{"type": "string"}` | Primitive string |
| `Int`, `Word`, `Int64` | `{"type": "integer"}` | Integral numbers |
| `Double`, `Float` | `{"type": "number"}` | Floating point |
| `Bool` | `{"type": "boolean"}` | True/False |
| `Maybe a` | Inner type `a` | **Omitted** from `required` properties list |
| `[a]` | `{"type": "array", "items": ...}` | List of items |
| Record types | `{"type": "object", "properties": ...}` | Nested objects |
| Nullary Sum types | `{"type": "string", "enum": [...]}` | String enums |

---

## Method 2: Manual `SchemaBuilder` DSL

For dynamic schemas or when you do not wish to define a dedicated Haskell record type, use the `SchemaBuilder` DSL (available directly from `Ollama`):

```haskell
import Ollama

personSchema :: Schema
personSchema = buildSchema $ emptyObject
  |+ ("name", JString)
  |+ ("age", JInteger)
  |+ ("skills", JArray JString)
  |! "name"    -- Mark 'name' as required
  |! "skills"  -- Mark 'skills' as required

main :: IO ()
main = do
  client <- defaultClient
  let req = (generateRequest "qwen3.5:2b" "Extract person details.")
        { genFormat = Just (SchemaFormat personSchema) }
  res <- generate client req
  ...
```
