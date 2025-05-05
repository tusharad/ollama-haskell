{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Create
  ( -- * Create Model API
    createModel
  , createModelOps
  ) where

import Control.Monad (unless)
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.HTTP.Client

-- TODO: Add Options parameter
-- TODO: Add Context parameter
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
createModelOps ::
  -- | Model Name
  Text ->
  -- | Model File
  Maybe Text ->
  -- | Stream
  Maybe Bool ->
  -- | Path
  Maybe FilePath ->
  IO ()
createModelOps
  modelName
  modelFile_
  stream_
  path_ =
    do
      let url = defaultOllamaUrl
      manager <- newManager defaultManagerSettings
      initialRequest <- parseRequest $ T.unpack (url <> "/api/create")
      let reqBody =
            CreateModelOps
              { name = modelName
              , modelFile = modelFile_
              , stream = stream_
              , path = path_
              }
          request =
            initialRequest
              { method = "POST"
              , requestBody = RequestBodyLBS $ encode reqBody
              }
      withResponse request manager $ \response -> do
        let go = do
              bs <- brRead $ responseBody response
              let eRes =
                    eitherDecode (BSL.fromStrict bs) ::
                      Either String CreateModelResp
              case eRes of
                Left err -> do
                  putStrLn $ "Error: " <> err <> show bs
                Right res -> do
                  unless
                    (status res /= "success")
                    ( do
                        T.putStr $ status res
                        go
                    )
        go

{- | Create a new model
| Please note, if you specify both ModelFile and Path, ModelFile will be used.
-}
createModel ::
  -- | Model Name
  Text ->
  -- | Model File
  Maybe Text ->
  -- | Path
  Maybe FilePath ->
  IO ()
createModel modelName modelFile_ =
  createModelOps
    modelName
    modelFile_
    Nothing
