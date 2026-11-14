{- |
Module      : Ollama.API.Ps
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Running models endpoint (@/api/ps@).

@since 1.0.0.0
-}
module Ollama.API.Ps (
  listRunning,
  RunningModelsResponse (..),
  RunningModel (..),
  isModelRunning,
  getModelVramUsage,
  matchesRunningModel,
  findRunningModel,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value)
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (request)
import Ollama.Error (OllamaError)
import Ollama.Types.Common (ModelName (..))
import Ollama.Types.Model (RunningModel (..), RunningModelsResponse (..))

{- | List models currently loaded into memory (@GET /api/ps@).

@since 1.0.0.0
-}
listRunning :: (MonadIO m) => OllamaClient -> m (Either OllamaError RunningModelsResponse)
listRunning client = request client "GET" "/api/ps" (Nothing :: Maybe Value)

{- | Check whether a 'ModelName' matches a loaded 'RunningModel'.
Matches against exact name, exact model identifier, or normalized @:latest@ tags.

@since 0.4.3.0
-}
matchesRunningModel :: ModelName -> RunningModel -> Bool
matchesRunningModel target rm =
  rmName rm == target
    || rmModel rm == target
    || unModelName (rmName rm) == targetWithLatest
    || unModelName (rmModel rm) == targetWithLatest
    || (hasLatest target && unModelName (rmName rm) == stripLatest target)
  where
    targetText = unModelName target
    targetWithLatest = if ":" `T.isInfixOf` targetText then targetText else targetText <> ":latest"
    hasLatest m = ":latest" `T.isSuffixOf` unModelName m
    stripLatest m = T.dropEnd 7 (unModelName m)

{- | Pure helper to find a running model by name in a list of loaded running models.

@since 0.4.3.0
-}
findRunningModel :: ModelName -> [RunningModel] -> Maybe RunningModel
findRunningModel target = find (matchesRunningModel target)

{- | Check if a model is currently loaded into memory (@GET /api/ps@).

@since 0.4.3.0
-}
isModelRunning :: (MonadIO m) => OllamaClient -> ModelName -> m (Either OllamaError Bool)
isModelRunning client target = do
  res <- listRunning client
  pure $ case res of
    Left err -> Left err
    Right (RunningModelsResponse models) ->
      Right (isJust (findRunningModel target models))

{- | Retrieve the GPU VRAM usage in bytes for a loaded model (@GET /api/ps@).
Returns 'Nothing' if the model is not currently loaded in memory.

@since 0.4.3.0
-}
getModelVramUsage ::
  (MonadIO m) => OllamaClient -> ModelName -> m (Either OllamaError (Maybe Int64))
getModelVramUsage client target = do
  res <- listRunning client
  pure $ case res of
    Left err -> Left err
    Right (RunningModelsResponse models) ->
      Right (rmSizeVram <$> findRunningModel target models)
