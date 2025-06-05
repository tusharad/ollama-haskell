{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Data.Ollama.Create
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Functionality for creating new models in the Ollama client.

This module provides functions to create a new model in the Ollama API using either a model file
content or a file path. It includes both an IO-based function ('createModel') and a monadic version
('createModelM') for use in 'MonadIO' contexts. The create operation is performed via a POST request
to the "/api//pull" endpoint, with streaming support for progress updates.

Note: If both 'modelFile' and 'path' are provided, 'modelFile' takes precedence.

Example:

>>> createModel "myModel" (Just "FROM llama3\nPARAMETER temperature 0.8") (Just True) Nothing Nothing
Creating model...
Completed
-}
module Data.Ollama.Create
  ( -- * Create Model API
    createModel
  , createModelM
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Ollama.Common.Config (OllamaConfig)
import Data.Ollama.Common.Types (HasDone (getDone))
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)

-- | Configuration for creating a new model.
data CreateModelOps = CreateModelOps
  { name :: !Text
  -- ^ The name of the model to create.
  , modelFile :: !(Maybe Text)
  -- ^ Optional model file content (e.g., Modelfile text). Takes precedence over 'path'.
  , stream :: !(Maybe Bool)
  -- ^ Optional flag to enable streaming progress updates.
  , path :: !(Maybe FilePath)
  -- ^ Optional file path to a Modelfile.
  }
  deriving (Show, Eq)

-- | Response type for model creation.
newtype CreateModelResp
  = -- | The status of the create operation (e.g., "success").
    CreateModelResp {status :: Text}
  deriving (Show, Eq)

instance HasDone CreateModelResp where
  getDone CreateModelResp {..} = status /= "success"

instance ToJSON CreateModelOps where
  toJSON
    ( CreateModelOps
        name_
        modelFile_
        stream_
        path_
      ) =
      object
        [ "name" .= name_
        , "modelfile" .= modelFile_
        , "stream" .= stream_
        , "path" .= path_
        ]

instance FromJSON CreateModelResp where
  parseJSON = withObject "CreateModelResp" $ \v ->
    CreateModelResp
      <$> v .: "status"

{- | Creates a new model using either model file content or a file path.

Sends a POST request to the "/api//pull" endpoint to create a model with the specified name.
The model can be defined either by 'modelFile' (Modelfile content as text) or 'path' (file path to a Modelfile).
If both are provided, 'modelFile' is used. Supports streaming progress updates if 'stream' is 'Just True'.
Prints progress messages to the console during creation.
-}
createModel ::
  -- | Model name
  Text ->
  -- | Optional model file content
  Maybe Text ->
  -- | Optional streaming flag
  Maybe Bool ->
  -- | Optional file path to a Modelfile
  Maybe FilePath ->
  -- | Optional 'OllamaConfig' (defaults to 'defaultOllamaConfig' if 'Nothing')
  Maybe OllamaConfig ->
  IO ()
createModel
  modelName
  modelFile_
  stream_
  path_
  mbConfig =
    void $
      withOllamaRequest
        "/api//pull"
        "POST"
        ( Just $
            CreateModelOps
              { name = modelName
              , modelFile = modelFile_
              , stream = stream_
              , path = path_
              }
        )
        mbConfig
        (commonStreamHandler onToken onComplete)
    where
      onToken :: CreateModelResp -> IO ()
      onToken _ = do
        putStrLn "Creating model..."

      onComplete :: IO ()
      onComplete = putStrLn "Completed"

{- | MonadIO version of 'createModel' for use in monadic contexts.

Lifts the 'createModel' function into a 'MonadIO' context, allowing it to be used in monadic computations.
-}
createModelM ::
  MonadIO m =>
  Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe FilePath ->
  Maybe OllamaConfig ->
  m ()
createModelM m mf s p mbCfg = liftIO $ createModel m mf s p mbCfg
