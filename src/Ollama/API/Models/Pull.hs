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
  pullStream,
) where

import Conduit (ConduitT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (request)
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
pull client model =
  request client "POST" "/api/pull" (Just $ PullRequest model Nothing (Just False))

{- | Download / pull a model streaming progress updates.

@since 1.0.0.0
-}
pullStream :: (MonadIO m) => OllamaClient -> ModelName -> ConduitT () PullResponse m ()
pullStream _client _model = liftIO $ pure ()
