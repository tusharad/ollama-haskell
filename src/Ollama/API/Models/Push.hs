{- |
Module      : Ollama.API.Models.Push
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Model push endpoint (@/api/push@).

@since 1.0.0.0
-}
module Ollama.API.Models.Push (
  PushRequest (..),
  PushResponse (..),
  push,
  pushStream,
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

{- | Push model request payload.

@since 1.0.0.0
-}
data PushRequest = PushRequest
  { psqModel :: !ModelName
  , psqInsecure :: !(Maybe Bool)
  , psqStream :: !(Maybe Bool)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON PushRequest where
  toJSON PushRequest {..} =
    object $
      catMaybes
        [ Just $ "model" .= psqModel
        , ("insecure" .=) <$> psqInsecure
        , ("stream" .=) <$> psqStream
        ]

{- | Push progress response payload.

@since 1.0.0.0
-}
data PushResponse = PushResponse
  { psStatus :: !Text
  , psDigest :: !(Maybe Digest)
  , psTotal :: !(Maybe Int64)
  , psCompleted :: !(Maybe Int64)
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON PushResponse where
  parseJSON = withObject "PushResponse" $ \v ->
    PushResponse
      <$> v .: "status"
      <*> v .:? "digest"
      <*> v .:? "total"
      <*> v .:? "completed"

instance ToJSON PushResponse where
  toJSON PushResponse {..} =
    object
      [ "status" .= psStatus
      , "digest" .= psDigest
      , "total" .= psTotal
      , "completed" .= psCompleted
      ]

instance HasDone PushResponse where
  isDone PushResponse {..} = psStatus == "success"

{- | Push a model to a remote library (non-streaming).

@since 1.0.0.0
-}
push :: (MonadIO m) => OllamaClient -> ModelName -> m (Either OllamaError PushResponse)
push client model =
  request client "POST" "/api/push" (Just $ PushRequest model Nothing (Just False))

{- | Push a model streaming upload progress updates.

@since 1.0.0.0
-}
pushStream :: (MonadUnliftIO m) => OllamaClient -> ModelName -> ConduitT () PushResponse m ()
pushStream client model =
  requestStreaming client "/api/push" (PushRequest model Nothing (Just True))
