{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module:      Data.Ollama.Generate
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Description: Generate functionality for Ollama client
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental
-}
module Data.Ollama.Generate
  ( -- * Generate Texts
    generate
  , defaultGenerateOps
  , generateJson
  , GenerateOps (..)
  , GenerateResponse (..)
  ) where

import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Maybe
import Data.Ollama.Common.Config (OllamaConfig (..))
import Data.Ollama.Common.Error (OllamaError (..))
import Data.Ollama.Common.Types (Format (..), GenerateResponse (..))
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

validateGenerateOps :: GenerateOps -> Either OllamaError GenerateOps
validateGenerateOps ops
  | T.null (modelName ops) = Left $ InvalidRequest "Model name cannot be empty"
  | T.null (prompt ops) = Left $ InvalidRequest "Prompt cannot be empty"
  | otherwise = Right ops

-- TODO: Add Context parameter

{- |
  Input type for generate functions. This data type represents all possible configurations
  that you can pass to the Ollama generate API.

  Example:

  > let ops = GenerateOps
  >         { modelName = "llama3.2"
  >         , prompt = "What is the meaning of life?"
  >         , suffix = Nothing
  >         , images = Nothing
  >         , format = Just "text"
  >         , system = Nothing
  >         , template = Nothing
  >         , stream = Nothing
  >         , raw = Just False
  >         , keepAlive = Just "yes"
  >         }
-}
data GenerateOps = GenerateOps
  { modelName :: !Text
  -- ^ The name of the model to be used for generation.
  , prompt :: !Text
  -- ^ The prompt text that will be provided to the model for generating a response.
  , suffix :: Maybe Text
  -- ^ An optional suffix to append to the generated text.
  , images :: !(Maybe [Text])
  -- ^ Optional list of base64 encoded images to include with the request.
  , format :: !(Maybe Format)
  -- ^ An optional format specifier for the response.
  -- ^ Since 0.1.3.0
  , system :: !(Maybe Text)
  -- ^ Optional system text that can be included in the generation context.
  , template :: !(Maybe Text)
  -- ^ An optional template to format the response.
  , stream :: !(Maybe (GenerateResponse -> IO (), IO ()))
  -- ^ An optional streaming function where the first function handles
  -- each chunk of response, and the second flushes the stream.
  , raw :: !(Maybe Bool)
  -- ^ An optional flag to return the raw response.
  , keepAlive :: !(Maybe Text)
  -- ^ Override default response timeout in minutes. Default = 15 minutes
  , options :: !(Maybe Value)
  -- ^ additional model parameters listed in the documentation for the Modelfile such as temperature
  -- ^ Since 0.1.3.0
  }

instance Show GenerateOps where
  show GenerateOps {..} =
    "GenerateOps { "
      <> "model : "
      <> T.unpack modelName
      <> ", prompt : "
      <> T.unpack prompt
      <> ", suffix : "
      <> show suffix
      <> ", images : "
      <> show images
      <> ", format : "
      <> show format
      <> ", system : "
      <> show system
      <> ", template : "
      <> show template
      <> ", stream : "
      <> "Stream functions"
      <> ", raw : "
      <> show raw
      <> ", keepAlive : "
      <> show keepAlive
      <> ", options : "
      <> show options

instance Eq GenerateOps where
  (==) a b =
    modelName a == modelName b
      && prompt a == prompt b
      && suffix a == suffix b
      && images a == images b
      && format a == format b
      && system a == system b
      && template a == template b
      && raw a == raw b
      && keepAlive a == keepAlive b
      && options a == options b

instance ToJSON GenerateOps where
  toJSON
    ( GenerateOps
        model
        prompt
        suffix
        images
        format
        system
        template
        stream
        raw
        keepAlive
        options
      ) =
      object
        [ "model" .= model
        , "prompt" .= prompt
        , "suffix" .= suffix
        , "images" .= images
        , "format" .= format
        , "system" .= system
        , "template" .= template
        , "stream" .= if isNothing stream then Just False else Just True
        , "raw" .= raw
        , "keep_alive" .= keepAlive
        , "options" .= options
        ]

{- |
A function to create a default 'GenerateOps' type with preset values.

Example:

> let ops = defaultGenerateOps
> generate ops

This will generate a response using the default configuration.
-}
defaultGenerateOps :: GenerateOps
defaultGenerateOps =
  GenerateOps
    { modelName = "llama3.2"
    , prompt = "what is 2+2"
    , suffix = Nothing
    , images = Nothing
    , format = Nothing
    , system = Nothing
    , template = Nothing
    , stream = Nothing
    , raw = Nothing
    , keepAlive = Nothing
    , options = Nothing
    }

{- |
Generate function that returns either a 'GenerateResponse' type or an error message.
It takes a 'GenerateOps' configuration and performs a request to the Ollama generate API.

Examples:

Basic usage without streaming:

> let ops = GenerateOps
>         { modelName = "llama3.2"
>         , prompt = "Tell me a joke."
>         , suffix = Nothing
>         , images = Nothing
>         , format = Nothing
>         , system = Nothing
>         , template = Nothing
>         , stream = Nothing
>         , raw = Nothing
>         , keepAlive = Nothing
>         }
> result <- generate ops
> case result of
>   Left errorMsg -> putStrLn ("Error: " ++ errorMsg)
>   Right response -> print response

Usage with streaming to print responses to the console:

> void $
>   generate
>     defaultGenerateOps
>       { modelName = "llama3.2"
>       , prompt = "what is functional programming?"
>       , stream = Just (T.putStr . response_, pure ())
>       }

In this example, the first function in the 'stream' tuple processes
each chunk of response by printing it,
and the second function is a simple no-op
flush.generate :: GenerateOps -> IO (Either String GenerateResponse)
-}
generate :: GenerateOps -> Maybe OllamaConfig -> IO (Either OllamaError GenerateResponse)
generate ops mbConfig =
  case validateGenerateOps ops of
    Left err -> pure $ Left err
    Right _ -> withOllamaRequest "/api/generate" "POST" (Just ops) mbConfig handler
  where
    handler = case stream ops of
      Nothing -> commonNonStreamingHandler
      Just (sc, fl) -> commonStreamHandler sc fl

{- |
 generateJson is a higher level function that takes generateOps (similar to generate) and also takes
 a Haskell type (that has To and From JSON instance) and returns the response in provided type.

 This function simply calls generate with extra prompt appended to it, telling LLM to return the
 response in certain JSON format and serializes the response. This function will be helpful when you
 want to use the LLM to do something programmatic.

 For Example:
  > let expectedJsonStrucutre = Example {
  >   sortedList = ["sorted List here"]
  > , wasListAlreadSorted = False
  > }
  > eRes2 <- generateJson
  >     defaultGenerateOps
  >      { modelName = "llama3.2"
  >     , prompt = "Sort given list: [4, 2 , 3, 67]. Also tell whether list was already sorted or not."
  >       }
  >     expectedJsonStrucutre
  >     Nothing
  > case eRes2 of
  >   Left e -> putStrLn e
  >   Right r -> print ("JSON response: " :: String, r)

Output:
  > ("JSON response: ",Example {sortedList = ["1","2","3","4"], wasListAlreadSorted = False})

Note: While Passing the type, construct the type that will help LLM understand the field better.
 For example, in the above example, the sortedList's value is written as "Sorted List here". This
 will help LLM understand context better.

 You can also provide number of retries in case the LLM field to return the response in correct JSON
 in first attempt.
-}
generateJson ::
  (ToJSON jsonResult, FromJSON jsonResult) =>
  GenerateOps ->
  -- | Haskell type that you want your result in
  Maybe OllamaConfig ->
  -- | Ollama Config
  jsonResult ->
  -- | Max retries
  Maybe Int ->
  IO (Either OllamaError jsonResult)
generateJson genOps@GenerateOps {..} mbConfig jsonStructure mMaxRetries = do
  let jsonHelperPrompt =
        "You are an AI that returns only JSON object. \n"
          <> "* Your output should be a JSON object that matches the following schema: \n"
          <> T.decodeUtf8 (BSL.toStrict $ encode jsonStructure)
          <> prompt
          <> "\n"
          <> "# How to treat the task:\n"
          <> "* Stricly follow the schema for the output.\n"
          <> "* Never return anything other than a JSON object.\n"
          <> "* Do not talk to the user.\n"
  generatedResponse <- generate genOps {prompt = jsonHelperPrompt} mbConfig
  case generatedResponse of
    Left err -> return $ Left err
    Right r -> do
      let bs = BSL.fromStrict . T.encodeUtf8 $ response_ r
      case eitherDecode bs of
        Left err -> do
          case mMaxRetries of
            Nothing -> return $ Left $ DecodeError err (show bs)
            Just n ->
              if n < 1
                then return $ Left $ DecodeError err (show bs)
                else generateJson genOps mbConfig jsonStructure (Just (n - 1))
        Right resultInType -> return $ Right resultInType

{- |
   Example usage of 'Ollama.generate' with a JSON schema format and options field.

   In this example we pass a JSON schema that expects an object with an integer field @age@.
   The options field is supplied as a JSON value.

   >>> import Data.Aeson (Value, object, (.=))
   >>> import Ollama (GenerateOps, defaultGenerateOps, SchemaFormat)
   >>> let x :: Value
   ...     x = object [ "type" .= ("object" :: String)
   ...                , "properties" .= object [ "age" .= object ["type" .= ("integer" :: String)] ]
   ...                ]
   >>> let opts :: Value
   ...     opts = object ["option" .= ("some value" :: String)]
   >>> generate defaultGenerateOps
   ...   { modelName = "llama3.2"
   ...   , prompt = "Ollama is 22 years old and is busy saving the world. Respond using JSON"
   ...   , format = Just (SchemaFormat x)
   ...   , options = opts
   ...   }
   Right (GenerateResponse {model = "llama3.2", createdAt = 2025-03-25 09:34:15.853417157 UTC,
                            response_ = "{\n    \"age\": 22\n}", done = True,
                            totalDuration = Just 6625631744, loadDuration = Just 2578791966,
                            promptEvalCount = Just 43, promptEvalDuration = Just 2983000000,
                            evalCount = Just 10, evalDuration = Just 1061000000})
-}
