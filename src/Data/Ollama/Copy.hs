{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Ollama.Copy
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Functionality for copying models in the Ollama client.

This module provides functions to copy a model from a source name to a destination name using the Ollama API.
It includes both an IO-based function ('copyModel') and a monadic version ('copyModelM') for use in
'MonadIO' contexts. The copy operation is performed via a POST request to the "/api//copy" endpoint.

Example:

>>> copyModel "gemma3" "gemma3-copy" Nothing
Right ()
-}
module Data.Ollama.Copy
  ( -- * Copy Model API
    copyModel
  , copyModelM
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Ollama.Common.Config (OllamaConfig (..))
import Data.Ollama.Common.Error (OllamaError)
import Data.Ollama.Common.Utils (nonJsonHandler, withOllamaRequest)
import Data.Text (Text)
import GHC.Generics

-- | Configuration for copying a model.
data CopyModelOps = CopyModelOps
  { source :: !Text
  -- ^ The name of the source model to copy.
  , destination :: !Text
  -- ^ The name of the destination model.
  }
  deriving (Show, Eq, Generic, ToJSON)

{- | Copies a model from a source name to a destination name.

Sends a POST request to the "/api//copy" endpoint with the source and destination model names.
Returns 'Right ()' on success or 'Left' with an 'OllamaError' on failure.
Example:

>>> copyModel "gemma3" "gemma3-copy" Nothing
Right ()
-}
copyModel ::
  -- | Source model name
  Text ->
  -- | Destination model name
  Text ->
  -- | Optional 'OllamaConfig' (defaults to 'defaultOllamaConfig' if 'Nothing')
  Maybe OllamaConfig ->
  IO (Either OllamaError ())
copyModel
  source_
  destination_
  mbConfig = do
    let reqBody = CopyModelOps {source = source_, destination = destination_}
    withOllamaRequest
      "/api//copy"
      "POST"
      (Just reqBody)
      mbConfig
      (fmap ((const ()) <$>) . nonJsonHandler)

{- | MonadIO version of 'copyModel' for use in monadic contexts.

Lifts the 'copyModel' function into a 'MonadIO' context, allowing it to be used in monadic computations.

Example:

>>> import Control.Monad.IO.Class
>>> runReaderT (copyModelM "gemma3" "gemma3-copy" Nothing) someContext
Right ()
-}
copyModelM :: MonadIO m => Text -> Text -> Maybe OllamaConfig -> m (Either OllamaError ())
copyModelM s d mbCfg = liftIO $ copyModel s d mbCfg
