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
  generateStructured,
  generateStream,
  accumulateGenerate,
  accumulateGenerateChunks,
  accumulateGenerateStream,
  evalTokensPerSecond,
  promptEvalTokensPerSecond,
  generateFullText,
) where

import Conduit (ConduitT)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (request, requestStreaming)
import Ollama.Error (OllamaError (..))
import Ollama.Streaming (HasDone (..), accumulateStream)
import Ollama.Types.Common (Base64Image, Duration, Logprob, ModelName (..), Think, tokensPerSecond)
import Ollama.Types.Format (Format, ToSchema, formatFor)
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
  , genLogprobs :: !(Maybe Bool)
  , genTopLogprobs :: !(Maybe Int)
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
        , ("logprobs" .=) <$> genLogprobs
        , ("top_logprobs" .=) <$> genTopLogprobs
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
    , genLogprobs = Nothing
    , genTopLogprobs = Nothing
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
  , grLogprobs :: !(Maybe [Logprob])
  , grCompleted :: !(Maybe Int)
  , grTotal :: !(Maybe Int)
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON GenerateResponse where
  parseJSON = withObject "GenerateResponse" $ \v ->
    GenerateResponse
      <$> v .: "model"
      <*> v .: "created_at"
      <*> v .:? "response" .!= ""
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
      <*> v .:? "logprobs"
      <*> v .:? "completed"
      <*> v .:? "total"

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
      , "logprobs" .= grLogprobs
      , "completed" .= grCompleted
      , "total" .= grTotal
      ]

instance HasDone GenerateResponse where
  isDone = grDone

{- | Non-streaming text completion API.

@since 1.0.0.0
-}
generate ::
  (MonadIO m) => OllamaClient -> GenerateRequest -> m (Either OllamaError GenerateResponse)
generate client req = request client "POST" "/api/generate" (Just req {genStream = Just False})

{- | Perform a text completion enforcing a JSON Schema derived from the type parameter @a@ via 'ToSchema',
and automatically decode the generated response string into @a@.

If the model response fails to decode into @a@, returns 'DecodeError' containing the parse error and raw bytes.

@since 0.4.3.0
-}
generateStructured ::
  forall a m.
  (MonadIO m, FromJSON a, ToSchema a) =>
  OllamaClient ->
  GenerateRequest ->
  m (Either OllamaError (a, GenerateResponse))
generateStructured client req = do
  let structuredReq = req {genFormat = Just (formatFor @a), genStream = Just False}
  res <- generate client structuredReq
  pure $ case res of
    Left err -> Left err
    Right raw ->
      let txt = grResponse raw
          bs = TE.encodeUtf8 txt
       in case eitherDecode (BSL.fromStrict bs) of
            Left parseErr -> Left (DecodeError (Text.pack parseErr) bs)
            Right val -> Right (val, raw)

{- | Streaming text completion API yielding 'GenerateResponse' chunks.

@since 1.0.0.0
-}
generateStream ::
  (MonadUnliftIO m) => OllamaClient -> GenerateRequest -> ConduitT () GenerateResponse m ()
generateStream client req = requestStreaming client "/api/generate" (req {genStream = Just True})

{- | Calculate generation throughput (eval tokens \/ second) from a 'GenerateResponse'.

@since 1.0.0.0
-}
evalTokensPerSecond :: GenerateResponse -> Maybe Double
evalTokensPerSecond GenerateResponse {..} =
  tokensPerSecond <$> grEvalCount <*> grEvalDuration

{- | Calculate prompt evaluation throughput (prompt tokens \/ second) from a 'GenerateResponse'.

@since 1.0.0.0
-}
promptEvalTokensPerSecond :: GenerateResponse -> Maybe Double
promptEvalTokensPerSecond GenerateResponse {..} =
  tokensPerSecond <$> grPromptEvalCount <*> grPromptEvalDuration

{- | Retrieve the complete text representation of a generate response, combining
thinking content and final response content if both are present.

This is especially helpful for reasoning models (e.g., DeepSeek-R1, Qwen 2.5/3.5)
where intermediate tokens are streamed or stored in 'grThinking'.

@since 0.4.2.0
-}
generateFullText :: GenerateResponse -> Text
generateFullText resp = case (grThinking resp, grResponse resp) of
  (Just th, r)
    | not (Text.null th) && not (Text.null r) -> th <> "\n\n" <> r
    | not (Text.null th) -> th
  (_, r) -> r

{- | Combine two consecutive 'GenerateResponse' chunks into an accumulated response.
Sequential text and thinking tokens are concatenated, and context, durations,
and completion flags from the latest chunk are maintained.

@since 0.4.3.0
-}
accumulateGenerate :: GenerateResponse -> GenerateResponse -> GenerateResponse
accumulateGenerate r1 r2 =
  r2
    { grModel = if Text.null (unModelName (grModel r1)) then grModel r2 else grModel r1
    , grCreatedAt = grCreatedAt r1
    , grResponse = grResponse r1 <> grResponse r2
    , grDone = grDone r1 || grDone r2
    , grDoneReason = grDoneReason r2 <|> grDoneReason r1
    , grContext = grContext r2 <|> grContext r1
    , grTotalDuration = grTotalDuration r2 <|> grTotalDuration r1
    , grLoadDuration = grLoadDuration r2 <|> grLoadDuration r1
    , grPromptEvalCount = grPromptEvalCount r2 <|> grPromptEvalCount r1
    , grPromptEvalDuration = grPromptEvalDuration r2 <|> grPromptEvalDuration r1
    , grEvalCount = grEvalCount r2 <|> grEvalCount r1
    , grEvalDuration = grEvalDuration r2 <|> grEvalDuration r1
    , grThinking = combineText (grThinking r1) (grThinking r2)
    , grImage = grImage r2 <|> grImage r1
    , grLogprobs = combineLogprobs (grLogprobs r1) (grLogprobs r2)
    , grCompleted = grCompleted r2 <|> grCompleted r1
    , grTotal = grTotal r2 <|> grTotal r1
    }
  where
    combineText Nothing t2 = t2
    combineText t1 Nothing = t1
    combineText (Just t1) (Just t2) = Just (t1 <> t2)

    combineLogprobs Nothing lp2 = lp2
    combineLogprobs lp1 Nothing = lp1
    combineLogprobs (Just lp1) (Just lp2) = Just (lp1 ++ lp2)

{- | Accumulate a list of 'GenerateResponse' chunks into a single unified 'GenerateResponse'.
Returns 'InvalidRequest' if the input list is empty.

@since 0.4.3.0
-}
accumulateGenerateChunks :: [GenerateResponse] -> Either OllamaError GenerateResponse
accumulateGenerateChunks [] = Left (InvalidRequest "Cannot accumulate empty stream of GenerateResponse")
accumulateGenerateChunks (g : gs) = Right (go g gs)
  where
    go acc [] = acc
    go acc (x : xs) =
      let acc' = accumulateGenerate acc x
       in acc' `seq` go acc' xs

{- | Fold over a streaming text generation conduit and accumulate all emitted chunks into
a single final 'GenerateResponse'.

@since 0.4.3.0
-}
accumulateGenerateStream ::
  (MonadUnliftIO m) =>
  ConduitT () GenerateResponse m () ->
  m (Either OllamaError GenerateResponse)
accumulateGenerateStream = accumulateStream accumulateGenerateChunks
