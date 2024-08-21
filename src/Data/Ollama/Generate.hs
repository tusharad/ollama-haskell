{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Generate (
  -- * Generate Texts 
  generate,
  generateOps,
  GenerateOps (..),
  GenerateResponse (..),
  -- * Generate Texts returning response
  generateOpsReturningResponse,
  generateReturningResponse
) where

import Control.Monad (unless)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (UTCTime)
import GHC.Int (Int64)
import Network.HTTP.Client

-- TODO: Add Options parameter
-- TODO: Add Context parameter
data GenerateOps = GenerateOps
  { model :: Text,
    prompt :: Text,
    suffix :: Maybe Text,
    images :: Maybe [Text], -- Base64 encoded
    format :: Maybe Text,
    system :: Maybe Text,
    template :: Maybe Text,
    stream :: Maybe Bool,
    raw :: Maybe Bool,
    keepAlive :: Maybe Text
  }
  deriving (Show, Eq)

-- TODO: Add Context Param
data GenerateResponse = GenerateResponse
  { model :: Text,
    createdAt :: UTCTime,
    response_ :: Text,
    done :: Bool,
    totalDuration :: Maybe Int64,
    loadDuration :: Maybe Int64,
    promptEvalCount :: Maybe Int64,
    promptEvalDuration :: Maybe Int64,
    evalCount :: Maybe Int64,
    evalDuration :: Maybe Int64
  }
  deriving (Show, Eq)

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
      ) =
      object
        [ "model" .= model,
          "prompt" .= prompt,
          "suffix" .= suffix,
          "images" .= images,
          "format" .= format,
          "system" .= system,
          "template" .= template,
          "stream" .= stream,
          "raw" .= raw,
          "keep_alive" .= keepAlive
        ]

instance FromJSON GenerateResponse where
  parseJSON = withObject "GenerateResponse" $ \v ->
    GenerateResponse
      <$> v .: "model"
      <*> v .: "created_at"
      <*> v .: "response"
      <*> v .: "done"
      <*> v .:? "total_duration"
      <*> v .:? "load_duration"
      <*> v .:? "prompt_eval_count"
      <*> v .:? "prompt_eval_duration"
      <*> v .:? "eval_count"
      <*> v .:? "eval_duration"

generateOps_ :: 
  Text ->
  Text ->
  Maybe Text ->
  Maybe [Text] ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Text -> 
  IO (Request,Manager)
generateOps_ modelName prompt suffix images format system template stream raw keepAlive = do
    let url = CU.host defaultOllama
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest $ T.unpack (url <> "/api/generate")
    let reqBody =
          GenerateOps
            { model = modelName,
              prompt = prompt,
              suffix = suffix,
              images = images,
              format = format,
              system = system,
              template = template,
              stream = stream,
              raw = raw,
              keepAlive = keepAlive
            }
        request =
          initialRequest
            { method = "POST",
              requestBody = RequestBodyLBS $ encode reqBody
            }
    pure (request,manager)

-- | Generate text from a given model with options
generateOps ::
  Text -> -- ^ Model Name
  Text -> -- ^ Prompt
  Maybe Text -> -- ^ Suffix
  Maybe [Text] ->  -- ^ Images
  Maybe Text -> -- ^ Format
  Maybe Text -> -- ^ System
  Maybe Text -> -- ^ Template
  Maybe Bool -> -- ^ Stream
  Maybe Bool -> -- ^ Raw
  Maybe Text -> -- ^ Keep Alive
  IO ()
generateOps
  modelName
  prompt
  suffix
  images
  format
  system
  template
  stream
  raw
  keepAlive = do
    (request,manager) <- generateOps_ modelName prompt suffix images format system template stream raw keepAlive
    withResponse request manager $ \response -> do
      let go = do
            bs <- brRead $ responseBody response
            let eRes = eitherDecode (BSL.fromStrict bs) :: Either String GenerateResponse
            case eRes of
              Left err -> do
                putStrLn $ "Error: " <> err
              Right res -> do
                unless
                  (done res)
                  ( do
                      T.putStr $ response_ res
                      go
                  )
      putStrLn "" -- newline after answer ends
      go

-- | Generate text from a given model
generate :: 
  Text -> -- ^ Model Name 
  Text ->  -- ^ Prompt
  IO ()
generate modelName prompt =
  generateOps
    modelName
    prompt
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

-- It is expected that while calling generateOpsReturningResponse, that stream is set to `false`.
-- | Generate text from a given model with extran options but returning the response.
generateOpsReturningResponse :: 
  Text -> -- ^ Model Name
  Text ->  -- ^ Prompt
  Maybe Text -> -- ^ Suffix
  Maybe [Text] ->  -- ^ Images
  Maybe Text ->  -- ^ Format
  Maybe Text ->  -- ^ System
  Maybe Text ->  -- ^ Template
  Maybe Bool ->  -- ^ Stream
  Maybe Bool ->  -- ^ Raw
  Maybe Text ->  -- ^ Keep Alive
  IO (Maybe GenerateResponse)
generateOpsReturningResponse modelName prompt suffix images format system template stream raw keepAlive = do
    (request,manager) <- generateOps_ modelName prompt suffix images format system template stream raw keepAlive
    resp <- httpLbs request manager
    let eRes = eitherDecode (responseBody resp) :: Either String GenerateResponse
    case eRes of
      Left err -> do
        putStrLn $ "Error: " <> err
        pure Nothing
      Right res -> do
        pure $ Just res

-- | Generate text from a given model but returning the response.
generateReturningResponse :: 
  Text -> -- ^ Model Name 
  Text ->  -- ^ Prompt
  IO (Maybe GenerateResponse)
generateReturningResponse modelName prompt = do
  generateOpsReturningResponse
    modelName
    prompt
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

