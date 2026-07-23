{- |
Module      : Ollama.Streaming
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Streaming pipeline utilities and HasDone class for completion streams.

@since 1.0.0.0
-}
module Ollama.Streaming (
  HasDone (..),
) where

import Ollama.Types.Model ()

{- | Typeclass for responses that indicate completion in a stream.

@since 1.0.0.0
-}
class HasDone a where
  isDone :: a -> Bool
