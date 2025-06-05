{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Ollama.Delete
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Functionality for deleting models in the Ollama client.

This module provides functions to delete a model from the Ollama server using its name. It includes
both an IO-based function ('deleteModel') and a monadic version ('deleteModelM') for use in
'MonadIO' contexts. The delete operation is performed via a DELETE request to the "/api//delete" endpoint.

Example:

>>> deleteModel "gemma3" Nothing
Right ()
-}
module Data.Ollama.Delete
  ( -- * Delete Model API
    deleteModel
  , deleteModelM
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Ollama.Common.Config (OllamaConfig (..))
import Data.Ollama.Common.Error (OllamaError)
import Data.Ollama.Common.Utils (nonJsonHandler, withOllamaRequest)
import Data.Text (Text)

-- | Request payload for deleting a model.
newtype DeleteModelReq
  = -- | The name of the model to delete.
    DeleteModelReq {name :: Text}
  deriving newtype (Show, Eq)

instance ToJSON DeleteModelReq where
  toJSON (DeleteModelReq name_) = object ["name" .= name_]

{- | Deletes a model from the Ollama server.

Sends a DELETE request to the "/api//delete" endpoint with the specified model name.
Returns 'Right ()' on success or 'Left' with an 'OllamaError' on failure.
-}
deleteModel ::
  -- | Model name to delete
  Text ->
  -- | Optional 'OllamaConfig' (defaults to 'defaultOllamaConfig' if 'Nothing')
  Maybe OllamaConfig ->
  IO (Either OllamaError ())
deleteModel modelName mbConfig = do
  let reqBody = DeleteModelReq {name = modelName}
  withOllamaRequest
    "/api//delete"
    "DELETE"
    (Just reqBody)
    mbConfig
    (fmap ((const ()) <$>) . nonJsonHandler)

{- | MonadIO version of 'deleteModel' for use in monadic contexts.

Lifts the 'deleteModel' function into a  context,
allowing it to be used in monadic computations.
-}
deleteModelM :: MonadIO m => Text -> Maybe OllamaConfig -> m (Either OllamaError ())
deleteModelM t mbCfg = liftIO $ deleteModel t mbCfg
