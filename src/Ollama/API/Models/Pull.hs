{- |
Module      : Ollama.API.Models.Pull
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Model pull endpoint (@/api/pull@).

@since 1.0.0.0
-}
module Ollama.API.Models.Pull (
  PullRequest (..),
  PullResponse (..),
  pull,
  pullWith,
  pullStream,
  pullStreamWith,
) where

import Conduit (ConduitT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (request, requestStreaming)
import Ollama.Error (OllamaError)
import Ollama.Streaming (HasDone (..))
import Ollama.Types.Common (Digest, ModelName)

{- | Pull model request configuration payload.

@since 1.0.0.0
-}
data PullRequest = PullRequest
  { prqModel :: !ModelName
  , prqInsecure :: !(Maybe Bool)
  , prqStream :: !(Maybe Bool)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON PullRequest where
  toJSON PullRequest {..} =
    object $
      catMaybes
        [ Just $ "model" .= prqModel
        , ("insecure" .=) <$> prqInsecure
        , ("stream" .=) <$> prqStream
        ]

{- | Pull progress/status response payload.

@since 1.0.0.0
-}
data PullResponse = PullResponse
  { prStatus :: !Text
  , prDigest :: !(Maybe Digest)
  , prTotal :: !(Maybe Int64)
  , prCompleted :: !(Maybe Int64)
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON PullResponse where
  parseJSON = withObject "PullResponse" $ \v ->
    PullResponse
      <$> v .: "status"
      <*> v .:? "digest"
      <*> v .:? "total"
      <*> v .:? "completed"

instance ToJSON PullResponse where
  toJSON PullResponse {..} =
    object
      [ "status" .= prStatus
      , "digest" .= prDigest
      , "total" .= prTotal
      , "completed" .= prCompleted
      ]

instance HasDone PullResponse where
  isDone PullResponse {..} = prStatus == "success"

{- | Download / pull a model (non-streaming, blocks until complete).

@since 1.0.0.0
-}
pull :: (MonadIO m) => OllamaClient -> ModelName -> m (Either OllamaError PullResponse)
pull client model = pullWith client (PullRequest model Nothing (Just False))

{- | Download / pull a model with custom request options (e.g. insecure) (non-streaming).

@since 0.4.2.0
-}
pullWith :: (MonadIO m) => OllamaClient -> PullRequest -> m (Either OllamaError PullResponse)
pullWith client req = request client "POST" "/api/pull" (Just req {prqStream = Just False})

{- | Download / pull a model streaming progress updates.

@since 1.0.0.0
-}
pullStream :: (MonadUnliftIO m) => OllamaClient -> ModelName -> ConduitT () PullResponse m ()
pullStream client model = pullStreamWith client (PullRequest model Nothing (Just True))

{- | Download / pull a model with custom request options streaming progress updates.

@since 0.4.2.0
-}
pullStreamWith :: (MonadUnliftIO m) => OllamaClient -> PullRequest -> ConduitT () PullResponse m ()
pullStreamWith client req = requestStreaming client "/api/pull" (req {prqStream = Just True})
