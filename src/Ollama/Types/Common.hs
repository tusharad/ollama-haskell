{- |
Module      : Ollama.Types.Common
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Common newtypes and domain primitives for the Ollama API.

@since 3.0.0.0
-}
module Ollama.Types.Common (
  ModelName (..),
  mkModelName,
  Digest (..),
  Base64Image (..),
  Duration (..),
  durationToSeconds,
  durationToMillis,
  Version (..),
  Think (..),
  ThinkingLevel (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Aeson.Types (typeMismatch)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

{- | Model name following the @model:tag@ format.

@since 3.0.0.0
-}
newtype ModelName = ModelName {unModelName :: Text}
  deriving newtype (Eq, Ord, Show, IsString, ToJSON, FromJSON, Hashable)

{- | Smart constructor that validates that a model name is non-empty.

@since 3.0.0.0
-}
mkModelName :: Text -> Either Text ModelName
mkModelName t
  | T.null t = Left "Model name cannot be empty"
  | otherwise = Right (ModelName t)

{- | SHA256 digest of a layer blob.

@since 3.0.0.0
-}
newtype Digest = Digest {unDigest :: Text}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, Hashable)

{- | Base64-encoded image data for multimodal inputs.

@since 3.0.0.0
-}
newtype Base64Image = Base64Image {unBase64Image :: Text}
  deriving newtype (Eq, Show, ToJSON, FromJSON)

{- | Duration in nanoseconds as returned by the Ollama API.

@since 3.0.0.0
-}
newtype Duration = Duration {durationNanos :: Int64}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, Num)

{- | Convert duration nanoseconds to seconds.

@since 3.0.0.0
-}
durationToSeconds :: Duration -> Double
durationToSeconds (Duration ns) = fromIntegral ns / 1e9

{- | Convert duration nanoseconds to milliseconds.

@since 3.0.0.0
-}
durationToMillis :: Duration -> Double
durationToMillis (Duration ns) = fromIntegral ns / 1e6

{- | Ollama server engine version string.

@since 3.0.0.0
-}
newtype Version = Version {unVersion :: Text}
  deriving newtype (Eq, Show, ToJSON, FromJSON)

{- | Thinking level settings for reasoning models.

@since 3.0.0.0
-}
data ThinkingLevel = ThinkLow | ThinkMedium | ThinkHigh | ThinkMax
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance ToJSON ThinkingLevel where
  toJSON ThinkLow = String "low"
  toJSON ThinkMedium = String "medium"
  toJSON ThinkHigh = String "high"
  toJSON ThinkMax = String "max"

instance FromJSON ThinkingLevel where
  parseJSON = withText "ThinkingLevel" $ \case
    "low" -> pure ThinkLow
    "medium" -> pure ThinkMedium
    "high" -> pure ThinkHigh
    "max" -> pure ThinkMax
    other -> fail $ "Unknown thinking level: " <> T.unpack other

{- | Controls whether a thinking/reasoning model outputs its thoughts.

@since 3.0.0.0
-}
data Think
  = ThinkEnabled
  | ThinkDisabled
  | ThinkLevel !ThinkingLevel
  deriving stock (Eq, Show, Generic)

instance ToJSON Think where
  toJSON ThinkEnabled = Bool True
  toJSON ThinkDisabled = Bool False
  toJSON (ThinkLevel lvl) = toJSON lvl

instance FromJSON Think where
  parseJSON (Bool True) = pure ThinkEnabled
  parseJSON (Bool False) = pure ThinkDisabled
  parseJSON (String s) = ThinkLevel <$> parseJSON (String s)
  parseJSON v = typeMismatch "Think" v
