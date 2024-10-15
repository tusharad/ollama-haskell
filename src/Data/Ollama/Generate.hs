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

{- | Input type for generate functions. It takes all possible configs that
| you can pass to ollama generate api
-}
data GenerateOps = GenerateOps
  { modelName :: Text
  , prompt :: Text
  , suffix :: Maybe Text
  , images :: Maybe [Text] -- Base64 encoded
  , format :: Maybe Text
  , system :: Maybe Text
  , template :: Maybe Text
  , stream :: Maybe (GenerateResponse -> IO (), IO ())
  , raw :: Maybe Bool
  , keepAlive :: Maybe Text
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
      <> "Stram functions"
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

-- | Result type for generate function
data GenerateResponse = GenerateResponse
  { model :: Text
  , createdAt :: UTCTime
  , response_ :: Text
  , done :: Bool
  , totalDuration :: Maybe Int64
  , loadDuration :: Maybe Int64
  , promptEvalCount :: Maybe Int64
  , promptEvalDuration :: Maybe Int64
  , evalCount :: Maybe Int64
  , evalDuration :: Maybe Int64
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

-- | A function to create GenerateOps type.
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

-- | Generate function that returns Either GenerateResponse type or error message
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
