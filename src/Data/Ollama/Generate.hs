{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Ollama.Generate
  ( -- * Generate Texts
    generate
  , defaultGenerateOps
  , GenerateOps (..)
  , GenerateResponse (..)
  ) where

import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Maybe
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Int (Int64)
import Network.HTTP.Client

-- TODO: Add Options parameter
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
  { modelName :: Text
  -- ^ The name of the model to be used for generation.
  , prompt :: Text
  -- ^ The prompt text that will be provided to the model for generating a response.
  , suffix :: Maybe Text
  -- ^ An optional suffix to append to the generated text.
  , images :: Maybe [Text]
  -- ^ Optional list of base64 encoded images to include with the request.
  , format :: Maybe Text
  -- ^ An optional format specifier for the response.
  , system :: Maybe Text
  -- ^ Optional system text that can be included in the generation context.
  , template :: Maybe Text
  -- ^ An optional template to format the response.
  , stream :: Maybe (GenerateResponse -> IO (), IO ())
  -- ^ An optional streaming function where the first function handles each chunk of response, and the second flushes the stream.
  , raw :: Maybe Bool
  -- ^ An optional flag to return the raw response.
  , keepAlive :: Maybe Text
  -- ^ Optional text to specify keep-alive behavior.
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

instance Eq GenerateOps where
    (==) a b = 
        modelName a == modelName b &&
        prompt a == prompt b &&
        suffix a == suffix b &&
        images a == images b &&
        format a == format b &&
        system a == system b &&
        template a == template b &&
        raw a == raw b &&
        keepAlive a == keepAlive b

-- TODO: Add Context Param

-- | 
-- Result type for generate function containing the model's response and meta-information.
data GenerateResponse = GenerateResponse
  { model :: Text
  -- ^ The name of the model that generated the response.
  , createdAt :: UTCTime
  -- ^ The timestamp when the response was created.
  , response_ :: Text
  -- ^ The generated response from the model.
  , done :: Bool
  -- ^ A flag indicating whether the generation process is complete.
  , totalDuration :: Maybe Int64
  -- ^ Optional total duration in milliseconds for the generation process.
  , loadDuration :: Maybe Int64
  -- ^ Optional load duration in milliseconds for loading the model.
  , promptEvalCount :: Maybe Int64
  -- ^ Optional count of prompt evaluations during the generation process.
  , promptEvalDuration :: Maybe Int64
  -- ^ Optional duration in milliseconds for evaluating the prompt.
  , evalCount :: Maybe Int64
  -- ^ Optional count of evaluations during the generation process.
  , evalDuration :: Maybe Int64
  -- ^ Optional duration in milliseconds for evaluations during the generation process.
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

-- | 
-- A function to create a default 'GenerateOps' type with preset values.
-- 
-- Example:
--
-- > let ops = defaultGenerateOps
-- > generate ops
-- 
-- This will generate a response using the default configuration.
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
    }

-- | 
-- Generate function that returns either a 'GenerateResponse' type or an error message.
-- It takes a 'GenerateOps' configuration and performs a request to the Ollama generate API.
-- 
-- Examples:
--
-- Basic usage without streaming:
--
-- > let ops = GenerateOps 
-- >         { modelName = "llama3.2"
-- >         , prompt = "Tell me a joke."
-- >         , suffix = Nothing
-- >         , images = Nothing
-- >         , format = Nothing
-- >         , system = Nothing
-- >         , template = Nothing
-- >         , stream = Nothing
-- >         , raw = Nothing
-- >         , keepAlive = Nothing
-- >         }
-- > result <- generate ops
-- > case result of
-- >   Left errorMsg -> putStrLn ("Error: " ++ errorMsg)
-- >   Right response -> print response
--
-- Usage with streaming to print responses to the console:
--
-- > void $
-- >   generate
-- >     defaultGenerateOps
-- >       { modelName = "llama3.2"
-- >       , prompt = "what is functional programming?"
-- >       , stream = Just (T.putStr . response_, pure ())
-- >       }
--
-- In this example, the first function in the 'stream' tuple processes each chunk of response by printing it,
-- and the second function is a simple no-op flush.generate :: GenerateOps -> IO (Either String GenerateResponse)
generate :: GenerateOps -> IO (Either String GenerateResponse)
generate genOps = do
  let url = CU.host defaultOllama
  manager <-
    newManager -- Setting response timeout to 5 minutes, since llm takes time
      defaultManagerSettings {managerResponseTimeout = responseTimeoutMicro (5 * 60 * 1000000)}
  initialRequest <- parseRequest $ T.unpack (url <> "/api/generate")
  let reqBody = genOps
      request =
        initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode reqBody
          }
  withResponse request manager $ \response -> do
    let streamResponse sendChunk flush = do
          bs <- brRead $ responseBody response
          if BS.null bs
            then putStrLn "" >> pure (Left "")
            else do
              let eRes = eitherDecode (BSL.fromStrict bs) :: Either String GenerateResponse
              case eRes of
                Left e -> pure (Left e)
                Right r -> do
                  _ <- sendChunk r
                  _ <- flush
                  if done r then pure (Left "") else streamResponse sendChunk flush
    let genResponse op = do
          bs <- brRead $ responseBody response
          if bs == ""
            then do
              let eRes0 = eitherDecode (BSL.fromStrict op) :: Either String GenerateResponse
              case eRes0 of
                Left e -> pure (Left e)
                Right r -> pure (Right r)
            else genResponse (op <> bs)
    case stream genOps of
      Nothing -> genResponse ""
      Just (sendChunk, flush) -> streamResponse sendChunk flush
