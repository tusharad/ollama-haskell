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
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value)
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (request)
import Ollama.Error (OllamaError)
import Ollama.Types.Model (RunningModel (..), RunningModelsResponse (..))

{- | List models currently loaded into memory (@GET /api/ps@).

@since 1.0.0.0
-}
listRunning :: (MonadIO m) => OllamaClient -> m (Either OllamaError RunningModelsResponse)
listRunning client = request client "GET" "/api/ps" (Nothing :: Maybe Value)
