{- |
Module      : Ollama.Streaming
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Streaming pipeline utilities, stream combinators, and 'HasDone' class for API responses.

@since 1.0.0.0
-}
module Ollama.Streaming (
  HasDone (..),
  collectStream,
  foldStream,
  accumulateStream,
  decodeStreamLine,
) where

import Conduit (ConduitT, foldlC, runConduit, sinkList, (.|))
import Control.Exception (catch)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Ollama.Client.Internal (decodeStreamLine)
import Ollama.Error (OllamaError)

{- | Typeclass for responses that indicate completion in a stream.

@since 1.0.0.0
-}
class HasDone a where
  isDone :: a -> Bool

{- | Collect all yielded items from a streaming conduit into a list.

@since 1.0.0.0
-}
collectStream :: (MonadUnliftIO m) => ConduitT () a m () -> m [a]
collectStream stream = runConduit $ stream .| sinkList

{- | Fold over all yielded items from a streaming conduit.

@since 1.0.0.0
-}
foldStream :: (MonadUnliftIO m) => (b -> a -> b) -> b -> ConduitT () a m () -> m b
foldStream f acc stream = runConduit $ stream .| foldlC f acc

{- | Accumulate a stream using a chunk accumulation function, catching any 'OllamaError'.

@since 0.4.3.0
-}
accumulateStream ::
  (MonadUnliftIO m) =>
  ([a] -> Either OllamaError a) ->
  ConduitT () a m () ->
  m (Either OllamaError a)
accumulateStream accChunks stream =
  withRunInIO $ \run ->
    ( do
        chunks <- run (collectStream stream)
        pure (accChunks chunks)
    )
      `catch` \(e :: OllamaError) -> pure (Left e)
