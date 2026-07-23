-- |
-- Module      : Ollama.Types.Options
-- Copyright   : (c) 2024-2026 Tushar Adhatrao
-- License     : MIT
-- Maintainer  : tusharadhatrao@gmail.com
-- Stability   : stable
-- Portability : portable
--
-- Model parameters and runtime execution options.
--
-- @since 1.0.0.0
module Ollama.Types.Options
  ( ModelOptions (..)
  , defaultOptions
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:?), (.=))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Optional inference and hardware tuning parameters.
--
-- @since 1.0.0.0
data ModelOptions = ModelOptions
  { optNumKeep :: !(Maybe Int)
  , optSeed :: !(Maybe Int)
  , optNumPredict :: !(Maybe Int)
  , optDraftNumPredict :: !(Maybe Int)
  , optTopK :: !(Maybe Int)
  , optTopP :: !(Maybe Double)
  , optMinP :: !(Maybe Double)
  , optTypicalP :: !(Maybe Double)
  , optRepeatLastN :: !(Maybe Int)
  , optTemperature :: !(Maybe Double)
  , optRepeatPenalty :: !(Maybe Double)
  , optPresencePenalty :: !(Maybe Double)
  , optFrequencyPenalty :: !(Maybe Double)
  , optPenalizeNewline :: !(Maybe Bool)
  , optStop :: !(Maybe [Text])
  , optNuma :: !(Maybe Bool)
  , optNumCtx :: !(Maybe Int)
  , optNumBatch :: !(Maybe Int)
  , optNumGpu :: !(Maybe Int)
  , optMainGpu :: !(Maybe Int)
  , optUseMmap :: !(Maybe Bool)
  , optNumThread :: !(Maybe Int)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ModelOptions where
  toJSON opts =
    object $
      catMaybes
        [ ("num_keep" .=) <$> optNumKeep opts
        , ("seed" .=) <$> optSeed opts
        , ("num_predict" .=) <$> optNumPredict opts
        , ("draft_num_predict" .=) <$> optDraftNumPredict opts
        , ("top_k" .=) <$> optTopK opts
        , ("top_p" .=) <$> optTopP opts
        , ("min_p" .=) <$> optMinP opts
        , ("typical_p" .=) <$> optTypicalP opts
        , ("repeat_last_n" .=) <$> optRepeatLastN opts
        , ("temperature" .=) <$> optTemperature opts
        , ("repeat_penalty" .=) <$> optRepeatPenalty opts
        , ("presence_penalty" .=) <$> optPresencePenalty opts
        , ("frequency_penalty" .=) <$> optFrequencyPenalty opts
        , ("penalize_newline" .=) <$> optPenalizeNewline opts
        , ("stop" .=) <$> optStop opts
        , ("numa" .=) <$> optNuma opts
        , ("num_ctx" .=) <$> optNumCtx opts
        , ("num_batch" .=) <$> optNumBatch opts
        , ("num_gpu" .=) <$> optNumGpu opts
        , ("main_gpu" .=) <$> optMainGpu opts
        , ("use_mmap" .=) <$> optUseMmap opts
        , ("num_thread" .=) <$> optNumThread opts
        ]

instance FromJSON ModelOptions where
  parseJSON = withObject "ModelOptions" $ \v ->
    ModelOptions
      <$> v .:? "num_keep"
      <*> v .:? "seed"
      <*> v .:? "num_predict"
      <*> v .:? "draft_num_predict"
      <*> v .:? "top_k"
      <*> v .:? "top_p"
      <*> v .:? "min_p"
      <*> v .:? "typical_p"
      <*> v .:? "repeat_last_n"
      <*> v .:? "temperature"
      <*> v .:? "repeat_penalty"
      <*> v .:? "presence_penalty"
      <*> v .:? "frequency_penalty"
      <*> v .:? "penalize_newline"
      <*> v .:? "stop"
      <*> v .:? "numa"
      <*> v .:? "num_ctx"
      <*> v .:? "num_batch"
      <*> v .:? "num_gpu"
      <*> v .:? "main_gpu"
      <*> v .:? "use_mmap"
      <*> v .:? "num_thread"

-- | Default empty options (all settings default to server Modelfile values).
--
-- @since 1.0.0.0
defaultOptions :: ModelOptions
defaultOptions =
  ModelOptions
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
