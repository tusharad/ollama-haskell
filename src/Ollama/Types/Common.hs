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
  encodeBase64Image,
  readImageBase64,
  Duration (..),
  durationToSeconds,
  durationToMillis,
  tokensPerSecond,
  Version (..),
  Think (..),
  ThinkingLevel (..),
  TokenLogprob (..),
  Logprob (..),
) where

import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  Value (..),
  object,
  withObject,
  withText,
  (.:),
  (.:?),
  (.=),
 )
import Data.Aeson.Types (typeMismatch)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
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

{- | Encode raw binary image data into a 'Base64Image'.

@since 0.4.2.0
-}
encodeBase64Image :: ByteString -> Base64Image
encodeBase64Image bs = Base64Image (TE.decodeUtf8 (B64.encode bs))

{- | Read a local image file from disk and encode it as a 'Base64Image'.

@since 0.4.2.0
-}
readImageBase64 :: FilePath -> IO Base64Image
readImageBase64 path = encodeBase64Image <$> BS.readFile path

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

@since 1.0.0.0
-}
durationToMillis :: Duration -> Double
durationToMillis (Duration ns) = fromIntegral ns / 1e6

{- | Calculate tokens per second (tokens\/s) given a token count and a 'Duration'.

@since 1.0.0.0
-}
tokensPerSecond :: Int -> Duration -> Double
tokensPerSecond count dur =
  let secs = durationToSeconds dur
   in if secs <= 0 then 0.0 else fromIntegral count / secs

{- | Ollama server engine version string.

@since 3.0.0.0
-}
newtype Version = Version {unVersion :: Text}
  deriving stock (Eq, Show, Generic)

instance FromJSON Version where
  parseJSON = \case
    String s -> pure $ Version s
    Object v -> Version <$> v .: "version"
    v -> typeMismatch "Version" v

instance ToJSON Version where
  toJSON (Version s) = object ["version" .= s]

{- | Thinking level settings for reasoning models. Supports standard levels
as well as model-defined custom thinking levels.

@since 3.0.0.0
-}
data ThinkingLevel
  = ThinkLow
  | ThinkMedium
  | ThinkHigh
  | ThinkMax
  | ThinkCustom !Text
  deriving stock (Eq, Show, Generic)

instance ToJSON ThinkingLevel where
  toJSON ThinkLow = String "low"
  toJSON ThinkMedium = String "medium"
  toJSON ThinkHigh = String "high"
  toJSON ThinkMax = String "max"
  toJSON (ThinkCustom custom) = String custom

instance FromJSON ThinkingLevel where
  parseJSON = withText "ThinkingLevel" $ \case
    "low" -> pure ThinkLow
    "medium" -> pure ThinkMedium
    "high" -> pure ThinkHigh
    "max" -> pure ThinkMax
    other -> pure (ThinkCustom other)

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

{- | Individual token log probability.

@since 0.4.2.0
-}
data TokenLogprob = TokenLogprob
  { tlpToken :: !Text
  , tlpLogprob :: !Double
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON TokenLogprob where
  toJSON TokenLogprob {..} =
    object
      [ "token" .= tlpToken
      , "logprob" .= tlpLogprob
      ]

instance FromJSON TokenLogprob where
  parseJSON = withObject "TokenLogprob" $ \v ->
    TokenLogprob
      <$> v .: "token"
      <*> v .: "logprob"

{- | Log probability information for a generated token and optional top alternatives.

@since 0.4.2.0
-}
data Logprob = Logprob
  { lpToken :: !Text
  , lpLogprob :: !Double
  , lpTopLogprobs :: !(Maybe [TokenLogprob])
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Logprob where
  toJSON Logprob {..} =
    object $
      catMaybes
        [ Just ("token" .= lpToken)
        , Just ("logprob" .= lpLogprob)
        , ("top_logprobs" .=) <$> lpTopLogprobs
        ]

instance FromJSON Logprob where
  parseJSON = withObject "Logprob" $ \v ->
    Logprob
      <$> v .: "token"
      <*> v .: "logprob"
      <*> v .:? "top_logprobs"
