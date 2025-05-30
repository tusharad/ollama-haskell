{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

-- TODO: Add Options parameter
data CreateModelOps = CreateModelOps
  { name :: !Text
  , modelFile :: !(Maybe Text)
  , stream :: !(Maybe Bool)
  , path :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

-- TODO: Add Context Param
newtype CreateModelResp = CreateModelResp {status :: Text}
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

{- | Create a new model either from ModelFile or Path
Please note, if you specify both ModelFile and Path, ModelFile will be used.
-}
createModel ::
  -- | Model Name
  Text ->
  -- | Model File
  Maybe Text ->
  -- | Stream
  Maybe Bool ->
  -- | Path
  Maybe FilePath ->
  -- | Ollama config
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
        "/api/pull"
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
        putStrLn $ "Creating model..."

      onComplete :: IO ()
      onComplete = putStrLn "Completed"

createModelM ::
  MonadIO m =>
  Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe FilePath ->
  Maybe OllamaConfig ->
  m ()
createModelM m mf s p mbCfg = liftIO $ createModel m mf s p mbCfg
