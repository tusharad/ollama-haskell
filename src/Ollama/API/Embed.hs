-- |
-- Module      : Ollama.API.Embed
-- Copyright   : (c) 2024-2026 Tushar Adhatrao
-- License     : MIT
-- Maintainer  : tusharadhatrao@gmail.com
-- Stability   : stable
-- Portability : portable
--
-- Vector embeddings API endpoint (@/api/embed@).
--
-- @since 1.0.0.0
module Ollama.API.Embed
  ( EmbedRequest (..)
  , EmbedResponse (..)
  , embedRequest
  , embed
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (request)
import Ollama.Error (OllamaError)
import Ollama.Types.Common (Duration, ModelName)
import Ollama.Types.Options (ModelOptions)

-- | Embedding request payload for single text or batch list of texts.
--
-- @since 1.0.0.0
data EmbedRequest = EmbedRequest
  { embModel :: !ModelName
  , embInput :: !(Either Text [Text])
  , embTruncate :: !(Maybe Bool)
  , embOptions :: !(Maybe ModelOptions)
  , embKeepAlive :: !(Maybe Text)
  , embDimensions :: !(Maybe Int)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON EmbedRequest where
  toJSON EmbedRequest {..} =
    object $
      catMaybes
        [ Just $ "model" .= embModel
        , Just $ case embInput of
            Left single -> "input" .= single
            Right multiple -> "input" .= multiple
        , ("truncate" .=) <$> embTruncate
        , ("options" .=) <$> embOptions
        , ("keep_alive" .=) <$> embKeepAlive
        , ("dimensions" .=) <$> embDimensions
        ]

-- | Create an 'EmbedRequest' for a list of input texts.
--
-- @since 1.0.0.0
embedRequest :: ModelName -> [Text] -> EmbedRequest
embedRequest model inputs =
  EmbedRequest
    { embModel = model
    , embInput = Right inputs
    , embTruncate = Nothing
    , embOptions = Nothing
    , embKeepAlive = Nothing
    , embDimensions = Nothing
    }

-- | Embedding response payload containing vector embeddings.
--
-- @since 1.0.0.0
data EmbedResponse = EmbedResponse
  { erModel :: !ModelName
  , erEmbeddings :: ![[Double]]
  , erTotalDuration :: !(Maybe Duration)
  , erLoadDuration :: !(Maybe Duration)
  , erPromptEvalCount :: !(Maybe Int)
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON EmbedResponse where
  parseJSON = withObject "EmbedResponse" $ \v ->
    EmbedResponse
      <$> v .: "model"
      <*> v .: "embeddings"
      <*> v .:? "total_duration"
      <*> v .:? "load_duration"
      <*> v .:? "prompt_eval_count"

instance ToJSON EmbedResponse where
  toJSON EmbedResponse {..} =
    object
      [ "model" .= erModel
      , "embeddings" .= erEmbeddings
      , "total_duration" .= erTotalDuration
      , "load_duration" .= erLoadDuration
      , "prompt_eval_count" .= erPromptEvalCount
      ]

-- | Generate vector embeddings.
--
-- @since 1.0.0.0
embed :: MonadIO m => OllamaClient -> EmbedRequest -> m (Either OllamaError EmbedResponse)
embed client req = request client "POST" "/api/embed" (Just req)
