-- |
-- Module      : Ollama.API.Blobs
-- Copyright   : (c) 2024-2026 Tushar Adhatrao
-- License     : MIT
-- Maintainer  : tusharadhatrao@gmail.com
-- Stability   : stable
-- Portability : portable
--
-- Blob management endpoints (@/api/blobs/:digest@).
--
-- @since 1.0.0.0
module Ollama.API.Blobs
  ( checkBlob
  , pushBlob
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (requestRaw)
import Ollama.Error (OllamaError (..))
import Ollama.Types.Common (Digest (..))

-- | Check if a blob exists on the server (@HEAD /api/blobs/:digest@).
--
-- @since 1.0.0.0
checkBlob :: MonadIO m => OllamaClient -> Digest -> m (Either OllamaError Bool)
checkBlob client (Digest d) = do
  res <- requestRaw client "HEAD" ("/api/blobs/" <> d) Nothing
  pure $ case res of
    Right _ -> Right True
    Left (ApiError 404 _) -> Right False
    Left err -> Left err

-- | Push / upload a file blob to the server (@POST /api/blobs/:digest@).
--
-- @since 1.0.0.0
pushBlob :: MonadIO m => OllamaClient -> Digest -> ByteString -> m (Either OllamaError ())
pushBlob client (Digest d) payload = do
  res <- requestRaw client "POST" ("/api/blobs/" <> d) (Just payload)
  pure $ fmap (const ()) res
