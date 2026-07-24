{- |
Module      : Ollama.API.Models.Create
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Model creation endpoint (@/api/create@).

@since 1.0.0.0
-}
module Ollama.API.Models.Create (
  CreateRequest (..),
  CreateResponse (..),
  QuantizationType (..),
  createModel,
  createModelStream,
  defaultCreateRequest,
) where

import Conduit (ConduitT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (request, requestStreaming)
import Ollama.Error (OllamaError)
import Ollama.Streaming (HasDone (..))
import Ollama.Types.Common (Digest, ModelName)
import Ollama.Types.Message (Message)
import Ollama.Types.Options (ModelOptions)

{- | Quantization precision options for model creation.

@since 1.0.0.0
-}
data QuantizationType = Q4_K_M | Q4_K_S | Q8_0
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance ToJSON QuantizationType where
  toJSON Q4_K_M = String "q4_K_M"
  toJSON Q4_K_S = String "q4_K_S"
  toJSON Q8_0 = String "q8_0"

instance FromJSON QuantizationType where
  parseJSON = withText "QuantizationType" $ \case
    "q4_K_M" -> pure Q4_K_M
    "q4_K_S" -> pure Q4_K_S
    "q8_0" -> pure Q8_0
    other -> fail $ "Invalid QuantizationType: " <> show other

{- | Model creation request configuration payload.

@since 1.0.0.0
-}
data CreateRequest = CreateRequest
  { crqModel :: !ModelName
  , crqFrom :: !(Maybe ModelName)
  , crqFiles :: !(Maybe (Map Text Digest))
  , crqAdapters :: !(Maybe (Map Text Digest))
  , crqTemplate :: !(Maybe Text)
  , crqRenderer :: !(Maybe Text)
  , crqParser :: !(Maybe Text)
  , crqLicense :: !(Maybe [Text])
  , crqSystem :: !(Maybe Text)
  , crqParameters :: !(Maybe ModelOptions)
  , crqMessages :: !(Maybe [Message])
  , crqStream :: !(Maybe Bool)
  , crqQuantize :: !(Maybe QuantizationType)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CreateRequest where
  toJSON CreateRequest {..} =
    object $
      catMaybes
        [ Just $ "model" .= crqModel
        , ("from" .=) <$> crqFrom
        , ("files" .=) <$> crqFiles
        , ("adapters" .=) <$> crqAdapters
        , ("template" .=) <$> crqTemplate
        , ("renderer" .=) <$> crqRenderer
        , ("parser" .=) <$> crqParser
        , ("license" .=) <$> crqLicense
        , ("system" .=) <$> crqSystem
        , ("parameters" .=) <$> crqParameters
        , ("messages" .=) <$> crqMessages
        , ("stream" .=) <$> crqStream
        , ("quantize" .=) <$> crqQuantize
        ]

{- | Smart constructor for a basic 'CreateRequest'.

@since 1.0.0.0
-}
defaultCreateRequest :: ModelName -> CreateRequest
defaultCreateRequest name =
  CreateRequest
    { crqModel = name
    , crqFrom = Nothing
    , crqFiles = Nothing
    , crqAdapters = Nothing
    , crqTemplate = Nothing
    , crqRenderer = Nothing
    , crqParser = Nothing
    , crqLicense = Nothing
    , crqSystem = Nothing
    , crqParameters = Nothing
    , crqMessages = Nothing
    , crqStream = Just False
    , crqQuantize = Nothing
    }

{- | Progress / status response during model creation.

@since 1.0.0.0
-}
data CreateResponse = CreateResponse
  { crsStatus :: !Text
  , crsDigest :: !(Maybe Digest)
  , crsTotal :: !(Maybe Int64)
  , crsCompleted :: !(Maybe Int64)
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON CreateResponse where
  parseJSON = withObject "CreateResponse" $ \v ->
    CreateResponse
      <$> v .: "status"
      <*> v .:? "digest"
      <*> v .:? "total"
      <*> v .:? "completed"

instance ToJSON CreateResponse where
  toJSON CreateResponse {..} =
    object
      [ "status" .= crsStatus
      , "digest" .= crsDigest
      , "total" .= crsTotal
      , "completed" .= crsCompleted
      ]

instance HasDone CreateResponse where
  isDone CreateResponse {..} = crsStatus == "success"

{- | Create a model non-streaming.

@since 1.0.0.0
-}
createModel :: (MonadIO m) => OllamaClient -> CreateRequest -> m (Either OllamaError CreateResponse)
createModel client req = request client "POST" "/api/create" (Just req {crqStream = Just False})

{- | Create a model streaming progress updates.

@since 1.0.0.0
-}
createModelStream ::
  (MonadUnliftIO m) => OllamaClient -> CreateRequest -> ConduitT () CreateResponse m ()
createModelStream client req = requestStreaming client "/api/create" (req {crqStream = Just True})
