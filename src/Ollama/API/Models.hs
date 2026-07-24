{- |
Module      : Ollama.API.Models
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Model management endpoints (list, show, copy, delete).

@since 1.0.0.0
-}
module Ollama.API.Models (
  -- * Listing
  listModels,
  ListResponse (..),

  -- * Show Info
  showModel,
  ShowRequest (..),
  ShowResponse (..),
  ShowModelInfo (..),

  -- * Copy
  copyModel,
  CopyRequest (..),

  -- * Delete
  deleteModel,
  DeleteRequest (..),
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ollama.Client (OllamaClient)
import Ollama.Client.Internal (request)
import Ollama.Error (OllamaError)
import Ollama.Types.Common (ModelName)
import Ollama.Types.Model (ListResponse (..), ModelDetails (..))

{- | Show model info request payload.

@since 1.0.0.0
-}
data ShowRequest = ShowRequest
  { srqModel :: !ModelName
  , srqVerbose :: !(Maybe Bool)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ShowRequest where
  toJSON ShowRequest {..} =
    object $
      catMaybes
        [ Just $ "model" .= srqModel
        , ("verbose" .=) <$> srqVerbose
        ]

{- | Detailed technical parameters from model metadata.

@since 1.0.0.0
-}
newtype ShowModelInfo = ShowModelInfo
  { modelInfoMap :: Map Text Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Model inspection response payload.

@since 1.0.0.0
-}
data ShowResponse = ShowResponse
  { srsModelfile :: !Text
  , srsParameters :: !(Maybe Text)
  , srsTemplate :: !(Maybe Text)
  , srsDetails :: !ModelDetails
  , srsModelInfo :: !(Maybe ShowModelInfo)
  , srsLicense :: !(Maybe Text)
  , srsCapabilities :: !(Maybe [Text])
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ShowResponse where
  parseJSON = withObject "ShowResponse" $ \v ->
    ShowResponse
      <$> v .:? "modelfile" .!= ""
      <*> v .:? "parameters"
      <*> v .:? "template"
      <*> v .:? "details" .!= ModelDetails Nothing "" "" [] "" ""
      <*> v .:? "model_info"
      <*> v .:? "license"
      <*> v .:? "capabilities"

instance ToJSON ShowResponse where
  toJSON ShowResponse {..} =
    object
      [ "modelfile" .= srsModelfile
      , "parameters" .= srsParameters
      , "template" .= srsTemplate
      , "details" .= srsDetails
      , "model_info" .= srsModelInfo
      , "license" .= srsLicense
      , "capabilities" .= srsCapabilities
      ]

{- | Copy model request payload.

@since 1.0.0.0
-}
data CopyRequest = CopyRequest
  { cpSource :: !ModelName
  , cpDestination :: !ModelName
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CopyRequest where
  toJSON CopyRequest {..} =
    object
      [ "source" .= cpSource
      , "destination" .= cpDestination
      ]

{- | Delete model request payload.

@since 1.0.0.0
-}
newtype DeleteRequest = DeleteRequest
  { delModel :: ModelName
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DeleteRequest where
  toJSON DeleteRequest {..} = object ["model" .= delModel]

{- | List installed local models (@GET /api/tags@).

@since 1.0.0.0
-}
listModels :: (MonadIO m) => OllamaClient -> m (Either OllamaError ListResponse)
listModels client = request client "GET" "/api/tags" (Nothing :: Maybe Value)

{- | Fetch information and Modelfile content for a model (@POST /api/show@).

@since 1.0.0.0
-}
showModel :: (MonadIO m) => OllamaClient -> ModelName -> m (Either OllamaError ShowResponse)
showModel client model = request client "POST" "/api/show" (Just $ ShowRequest model Nothing)

{- | Duplicate an existing model under a new tag (@POST /api/copy@).

@since 1.0.0.0
-}
copyModel :: (MonadIO m) => OllamaClient -> ModelName -> ModelName -> m (Either OllamaError ())
copyModel client src dst = request client "POST" "/api/copy" (Just $ CopyRequest src dst)

{- | Delete a local model (@DELETE /api/delete@).

@since 1.0.0.0
-}
deleteModel :: (MonadIO m) => OllamaClient -> ModelName -> m (Either OllamaError ())
deleteModel client model = request client "DELETE" "/api/delete" (Just $ DeleteRequest model)
