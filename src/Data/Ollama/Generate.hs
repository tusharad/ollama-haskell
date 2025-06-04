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
  , generateM
  , defaultGenerateOps
  , defaultOllamaConfig
  , validateGenerateOps
  , defaultModelOptions
  , GenerateOps (..)
  , GenerateResponse (..)
  , Format (..)
  , OllamaConfig (..)
  , ModelOptions (..)
  , OllamaError (..)
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Maybe
import Data.Ollama.Common.Config (OllamaConfig (..), defaultOllamaConfig)
import Data.Ollama.Common.Error (OllamaError (..))
import Data.Ollama.Common.Types (Format (..), GenerateResponse (..), ModelOptions (..))
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Text qualified as T

validateGenerateOps :: GenerateOps -> Either OllamaError GenerateOps
validateGenerateOps ops
  | T.null (modelName ops) = Left $ InvalidRequest "Model name cannot be empty"
  | T.null (prompt ops) = Left $ InvalidRequest "Prompt cannot be empty"
  | otherwise = Right ops

data GenerateOps = GenerateOps
  { modelName :: !Text
  -- ^ The name of the model to be used for generation.
  , prompt :: !Text
  -- ^ The prompt text that will be provided to the model for generating a response.
  , suffix :: Maybe Text
  -- ^ An optional suffix to append to the generated text. Not all models supports this.
  , images :: !(Maybe [Text])
  -- ^ Optional list of base64 encoded images to include with the request.
  , format :: !(Maybe Format)
  -- ^ An optional format specifier for the response.
  --
  -- @since 0.1.3.0
  , system :: !(Maybe Text)
  -- ^ Optional system text that can be included in the generation context.
  , template :: !(Maybe Text)
  -- ^ An optional template to format the response.
  , stream :: !(Maybe (GenerateResponse -> IO (), IO ()))
  -- ^ An optional streaming function where the first function handles
  -- each chunk of response, and the second flushes the stream.
  , raw :: !(Maybe Bool)
  -- ^ An optional flag to return the raw response.
  , keepAlive :: !(Maybe Int)
  -- ^ controls how long the model will stay loaded into memory following the request (default: 5m)
  , options :: !(Maybe ModelOptions)
  -- ^ additional model parameters listed in the documentation for the Modelfile such as temperature
  --
  -- @since 0.1.3.0
  , think :: !(Maybe Bool)
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
      <> ", think: "
      <> show think

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
      && think a == think b

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
        think
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
        , "think" .= think
        ]

defaultGenerateOps :: GenerateOps
defaultGenerateOps =
  GenerateOps
    { modelName = "gemma3"
    , prompt = ""
    , suffix = Nothing
    , images = Nothing
    , format = Nothing
    , system = Nothing
    , template = Nothing
    , stream = Nothing
    , raw = Nothing
    , keepAlive = Nothing
    , options = Nothing
    , think = Nothing
    }

generate :: GenerateOps -> Maybe OllamaConfig -> IO (Either OllamaError GenerateResponse)
generate ops mbConfig =
  case validateGenerateOps ops of
    Left err -> pure $ Left err
    Right _ -> withOllamaRequest "/api/generate" "POST" (Just ops) mbConfig handler
  where
    handler = case stream ops of
      Nothing -> commonNonStreamingHandler
      Just (sc, fl) -> commonStreamHandler sc fl

generateM ::
  MonadIO m =>
  GenerateOps -> Maybe OllamaConfig -> m (Either OllamaError GenerateResponse)
generateM ops mbCfg = liftIO $ generate ops mbCfg
