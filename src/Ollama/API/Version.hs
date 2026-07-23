{- |
Module      : Ollama.API.Version
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Version endpoint (@/api/version@).

@since 1.0.0.0
-}
module Ollama.API.Version (
  getVersion,
  Version (..),
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value)
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (request)
import Ollama.Error (OllamaError)
import Ollama.Types.Common (Version (..))

{- | Retrieve Ollama server engine version (@GET /api/version@).

@since 1.0.0.0
-}
getVersion :: (MonadIO m) => OllamaClient -> m (Either OllamaError Version)
getVersion client = request client "GET" "/api/version" (Nothing :: Maybe Value)
