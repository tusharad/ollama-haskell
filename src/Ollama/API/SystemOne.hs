{- |
Module      : Ollama.API.SystemOne
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

System One decision evaluation API endpoint (@/v1/systemone@).

Evaluates high-speed classification, probability, and scoring questions
with specialized decision models (e.g. @nimble@).

@since 0.4.2.0
-}
module Ollama.API.SystemOne (
  -- * Client Function
  systemOne,

  -- * Request & Response
  SystemOneRequest (..),
  systemOneRequest,
  SystemOneResponse (..),
  SystemOneUsage (..),

  -- * Content
  SystemOneContent (..),

  -- * Questions
  SystemOneQuestion (..),
  questionInstructions,
  choiceQuestion,
  noulQuestion,
  noulQuestionWithCriteria,
  scoreQuestion,

  -- * Answers
  SystemOneAnswer (..),
  ChoiceAnswer (..),
  ScoreAnswer (..),
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (request)
import Ollama.Error (OllamaError)
import Ollama.Types.Common (ModelName)

{- | State or context payload passed to System One, represented as text or structured JSON.

@since 0.4.2.0
-}
data SystemOneContent
  = StateText !Text
  | StateJson !Value
  deriving stock (Eq, Show, Generic)

instance IsString SystemOneContent where
  fromString = StateText . T.pack

instance ToJSON SystemOneContent where
  toJSON (StateText t) = String t
  toJSON (StateJson v) = v

instance FromJSON SystemOneContent where
  parseJSON (String t) = pure (StateText t)
  parseJSON v = pure (StateJson v)

{- | Specification of a question to evaluate against the state.

@since 0.4.2.0
-}
data SystemOneQuestion
  = ChoiceQuestion !SystemOneContent !(Map Text (Maybe Text))
  | NoulQuestion !SystemOneContent !(Maybe (Map Text Text))
  | ScoreQuestion !SystemOneContent ![Text]
  deriving stock (Eq, Show, Generic)

-- | Extract the instructions from any question variant.
questionInstructions :: SystemOneQuestion -> SystemOneContent
questionInstructions (ChoiceQuestion inst _) = inst
questionInstructions (NoulQuestion inst _) = inst
questionInstructions (ScoreQuestion inst _) = inst

instance ToJSON SystemOneQuestion where
  toJSON (ChoiceQuestion inst criteria) =
    object
      [ "type" .= ("choice" :: Text)
      , "instructions" .= inst
      , "criteria" .= criteria
      ]
  toJSON (NoulQuestion inst criteria) =
    object $
      [ "type" .= ("noul" :: Text)
      , "instructions" .= inst
      ]
        ++ maybe [] (\c -> ["criteria" .= c]) criteria
  toJSON (ScoreQuestion inst criteria) =
    object
      [ "type" .= ("score" :: Text)
      , "instructions" .= inst
      , "criteria" .= criteria
      ]

instance FromJSON SystemOneQuestion where
  parseJSON = withObject "SystemOneQuestion" $ \v -> do
    qType <- v .: "type"
    case (qType :: Text) of
      "choice" -> ChoiceQuestion <$> v .: "instructions" <*> v .: "criteria"
      "noul" -> NoulQuestion <$> v .: "instructions" <*> v .:? "criteria"
      "score" -> ScoreQuestion <$> v .: "instructions" <*> v .: "criteria"
      other -> fail $ "Unknown SystemOne question type: " <> T.unpack other

{- | Smart constructor for a multiple-choice question with candidate descriptions.

@since 0.4.2.0
-}
choiceQuestion :: SystemOneContent -> Map Text (Maybe Text) -> SystemOneQuestion
choiceQuestion = ChoiceQuestion

{- | Smart constructor for a binary probability (noul) question.

@since 0.4.2.0
-}
noulQuestion :: SystemOneContent -> SystemOneQuestion
noulQuestion instructions = NoulQuestion instructions Nothing

{- | Smart constructor for a binary probability (noul) question with custom true\/false descriptions.

@since 0.4.2.0
-}
noulQuestionWithCriteria :: SystemOneContent -> Map Text Text -> SystemOneQuestion
noulQuestionWithCriteria instructions criteria = NoulQuestion instructions (Just criteria)

{- | Smart constructor for an ordinal scoring question with descriptions from lowest to highest score.

@since 0.4.2.0
-}
scoreQuestion :: SystemOneContent -> [Text] -> SystemOneQuestion
scoreQuestion = ScoreQuestion

{- | Result of a multiple-choice question evaluation.

@since 0.4.2.0
-}
data ChoiceAnswer = ChoiceAnswer
  { choiceResult :: !Text
  , choiceProbabilities :: !(Map Text Double)
  , choiceConfidence :: !Double
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ChoiceAnswer where
  parseJSON = withObject "ChoiceAnswer" $ \v ->
    ChoiceAnswer
      <$> v .: "choice"
      <*> v .: "probabilities"
      <*> v .: "confidence"

instance ToJSON ChoiceAnswer where
  toJSON ChoiceAnswer {..} =
    object
      [ "choice" .= choiceResult
      , "probabilities" .= choiceProbabilities
      , "confidence" .= choiceConfidence
      ]

{- | Result of an ordinal scoring question evaluation.

@since 0.4.2.0
-}
data ScoreAnswer = ScoreAnswer
  { scoreValue :: !Double
  , scoreLegend :: !(Map Text Text)
  , scoreProbabilities :: !(Map Text Double)
  , scoreConfidence :: !Double
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ScoreAnswer where
  parseJSON = withObject "ScoreAnswer" $ \v ->
    ScoreAnswer
      <$> v .: "score"
      <*> v .: "legend"
      <*> v .: "probabilities"
      <*> v .: "confidence"

instance ToJSON ScoreAnswer where
  toJSON ScoreAnswer {..} =
    object
      [ "score" .= scoreValue
      , "legend" .= scoreLegend
      , "probabilities" .= scoreProbabilities
      , "confidence" .= scoreConfidence
      ]

{- | Answer returned by the model for a question.

@since 0.4.2.0
-}
data SystemOneAnswer
  = AnswerChoice !ChoiceAnswer
  | AnswerNoul !Double
  | AnswerScore !ScoreAnswer
  deriving stock (Eq, Show, Generic)

instance FromJSON SystemOneAnswer where
  parseJSON = withObject "SystemOneAnswer" $ \v -> do
    aType <- v .: "type"
    case (aType :: Text) of
      "choice" -> AnswerChoice <$> parseJSON (Object v)
      "noul" -> AnswerNoul <$> v .: "noul"
      "score" -> AnswerScore <$> parseJSON (Object v)
      other -> fail $ "Unknown SystemOne answer type: " <> T.unpack other

instance ToJSON SystemOneAnswer where
  toJSON (AnswerChoice ChoiceAnswer {..}) =
    object
      [ "type" .= ("choice" :: Text)
      , "choice" .= choiceResult
      , "probabilities" .= choiceProbabilities
      , "confidence" .= choiceConfidence
      ]
  toJSON (AnswerNoul p) =
    object
      [ "type" .= ("noul" :: Text)
      , "noul" .= p
      ]
  toJSON (AnswerScore ScoreAnswer {..}) =
    object
      [ "type" .= ("score" :: Text)
      , "score" .= scoreValue
      , "legend" .= scoreLegend
      , "probabilities" .= scoreProbabilities
      , "confidence" .= scoreConfidence
      ]

{- | Token usage statistics for System One requests.

@since 0.4.2.0
-}
data SystemOneUsage = SystemOneUsage
  { souInputTokens :: !Int
  , souOutputTokens :: !Int
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON SystemOneUsage where
  parseJSON = withObject "SystemOneUsage" $ \v ->
    SystemOneUsage <$> v .: "input_tokens" <*> v .: "output_tokens"

instance ToJSON SystemOneUsage where
  toJSON SystemOneUsage {..} =
    object
      [ "input_tokens" .= souInputTokens
      , "output_tokens" .= souOutputTokens
      ]

{- | Request payload for the System One evaluation endpoint.

@since 0.4.2.0
-}
data SystemOneRequest = SystemOneRequest
  { soModel :: !ModelName
  , soState :: !SystemOneContent
  , soQuestions :: !(Map Text SystemOneQuestion)
  , soKeepAlive :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SystemOneRequest where
  toJSON SystemOneRequest {..} =
    object $
      catMaybes
        [ Just ("model" .= soModel)
        , Just ("state" .= soState)
        , Just ("questions" .= soQuestions)
        , ("keep_alive" .=) <$> soKeepAlive
        ]

instance FromJSON SystemOneRequest where
  parseJSON = withObject "SystemOneRequest" $ \v ->
    SystemOneRequest
      <$> v .: "model"
      <*> v .: "state"
      <*> v .: "questions"
      <*> v .:? "keep_alive"

{- | Smart constructor for a 'SystemOneRequest'.

@since 0.4.2.0
-}
systemOneRequest :: ModelName -> SystemOneContent -> Map Text SystemOneQuestion -> SystemOneRequest
systemOneRequest model state questions =
  SystemOneRequest
    { soModel = model
    , soState = state
    , soQuestions = questions
    , soKeepAlive = Nothing
    }

{- | Response payload returned by the System One endpoint.

@since 0.4.2.0
-}
data SystemOneResponse = SystemOneResponse
  { sorModel :: !ModelName
  , sorAnswers :: !(Map Text SystemOneAnswer)
  , sorUsage :: !SystemOneUsage
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON SystemOneResponse where
  parseJSON = withObject "SystemOneResponse" $ \v ->
    SystemOneResponse
      <$> v .: "model"
      <*> v .: "answers"
      <*> v .: "usage"

instance ToJSON SystemOneResponse where
  toJSON SystemOneResponse {..} =
    object
      [ "model" .= sorModel
      , "answers" .= sorAnswers
      , "usage" .= sorUsage
      ]

{- | Score and evaluate choice, noul, and score questions with a local System One model (@POST /v1/systemone@).

@since 0.4.2.0
-}
systemOne ::
  (MonadIO m) =>
  OllamaClient ->
  SystemOneRequest ->
  m (Either OllamaError SystemOneResponse)
systemOne client req = request client "POST" "/v1/systemone" (Just req)
