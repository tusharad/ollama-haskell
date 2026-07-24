{- |
Module      : Ollama.API.Generate
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Text completion API endpoint (@/api/generate@).

@since 1.0.0.0
-}
module Ollama.API.Generate (
  GenerateRequest (..),
  GenerateResponse (..),
  generateRequest,
  generate,
  generateStream,
) where

import Conduit (ConduitT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (request, requestStreaming)
import Ollama.Error (OllamaError)
import Ollama.Streaming (HasDone (..))
import Ollama.Types.Common (Base64Image, Duration, ModelName, Think)
import Ollama.Types.Format (Format)
import Ollama.Types.Options (ModelOptions)

{- | Request payload for text generation completion.

@since 1.0.0.0
-}
data GenerateRequest = GenerateRequest
  { genModel :: !ModelName
  , genPrompt :: !Text
  , genSuffix :: !(Maybe Text)
  , genImages :: !(Maybe [Base64Image])
  , genFormat :: !(Maybe Format)
  , genOptions :: !(Maybe ModelOptions)
  , genSystem :: !(Maybe Text)
  , genTemplate :: !(Maybe Text)
  , genStream :: !(Maybe Bool)
  , genRaw :: !(Maybe Bool)
  , genKeepAlive :: !(Maybe Text)
  , genThink :: !(Maybe Think)
  , genWidth :: !(Maybe Int)
  , genHeight :: !(Maybe Int)
  , genSteps :: !(Maybe Int)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GenerateRequest where
  toJSON GenerateRequest {..} =
    object $
      catMaybes
        [ Just $ "model" .= genModel
        , Just $ "prompt" .= genPrompt
        , ("suffix" .=) <$> genSuffix
        , ("images" .=) <$> genImages
        , ("format" .=) <$> genFormat
        , ("options" .=) <$> genOptions
        , ("system" .=) <$> genSystem
        , ("template" .=) <$> genTemplate
        , ("stream" .=) <$> genStream
        , ("raw" .=) <$> genRaw
        , ("keep_alive" .=) <$> genKeepAlive
        , ("think" .=) <$> genThink
        , ("width" .=) <$> genWidth
        , ("height" .=) <$> genHeight
        , ("steps" .=) <$> genSteps
        ]

{- | Create a default 'GenerateRequest' for a model and prompt.

@since 1.0.0.0
-}
generateRequest :: ModelName -> Text -> GenerateRequest
generateRequest model prompt =
  GenerateRequest
    { genModel = model
    , genPrompt = prompt
    , genSuffix = Nothing
    , genImages = Nothing
    , genFormat = Nothing
    , genOptions = Nothing
    , genSystem = Nothing
    , genTemplate = Nothing
    , genStream = Just False
    , genRaw = Nothing
    , genKeepAlive = Nothing
    , genThink = Nothing
    , genWidth = Nothing
    , genHeight = Nothing
    , genSteps = Nothing
    }

{- | Response payload returned by text generation.

@since 1.0.0.0
-}
data GenerateResponse = GenerateResponse
  { grModel :: !ModelName
  , grCreatedAt :: !UTCTime
  , grResponse :: !Text
  , grDone :: !Bool
  , grDoneReason :: !(Maybe Text)
  , grContext :: !(Maybe [Int])
  , grTotalDuration :: !(Maybe Duration)
  , grLoadDuration :: !(Maybe Duration)
  , grPromptEvalCount :: !(Maybe Int)
  , grPromptEvalDuration :: !(Maybe Duration)
  , grEvalCount :: !(Maybe Int)
  , grEvalDuration :: !(Maybe Duration)
  , grThinking :: !(Maybe Text)
  , grImage :: !(Maybe Base64Image)
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON GenerateResponse where
  parseJSON = withObject "GenerateResponse" $ \v ->
    GenerateResponse
      <$> v .: "model"
      <*> v .: "created_at"
      <*> v .: "response"
      <*> v .: "done"
      <*> v .:? "done_reason"
      <*> v .:? "context"
      <*> v .:? "total_duration"
      <*> v .:? "load_duration"
      <*> v .:? "prompt_eval_count"
      <*> v .:? "prompt_eval_duration"
      <*> v .:? "eval_count"
      <*> v .:? "eval_duration"
      <*> v .:? "thinking"
      <*> v .:? "image"

instance ToJSON GenerateResponse where
  toJSON GenerateResponse {..} =
    object
      [ "model" .= grModel
      , "created_at" .= grCreatedAt
      , "response" .= grResponse
      , "done" .= grDone
      , "done_reason" .= grDoneReason
      , "context" .= grContext
      , "total_duration" .= grTotalDuration
      , "load_duration" .= grLoadDuration
      , "prompt_eval_count" .= grPromptEvalCount
      , "prompt_eval_duration" .= grPromptEvalDuration
      , "eval_count" .= grEvalCount
      , "eval_duration" .= grEvalDuration
      , "thinking" .= grThinking
      , "image" .= grImage
      ]

instance HasDone GenerateResponse where
  isDone = grDone

{- | Non-streaming text completion API.

@since 1.0.0.0
-}
generate ::
  (MonadIO m) => OllamaClient -> GenerateRequest -> m (Either OllamaError GenerateResponse)
generate client req = request client "POST" "/api/generate" (Just req {genStream = Just False})

{- | Streaming text completion API yielding 'GenerateResponse' chunks.

@since 1.0.0.0
-}
generateStream :: (MonadUnliftIO m) => OllamaClient -> GenerateRequest -> ConduitT () GenerateResponse m ()
generateStream client req = requestStreaming client "/api/generate" (req {genStream = Just True})
