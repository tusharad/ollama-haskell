module Test.Ollama.Property.StructuredAndTools (tests) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ollama
import Ollama.Types.Message qualified as M
import Test.Ollama.Property.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck

data SampleRecord = SampleRecord
  { sampleKey :: !Text
  , sampleNum :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Arbitrary SampleRecord where
  arbitrary = (SampleRecord . unModelName <$> arbitrary) <*> arbitrary

tests :: TestTree
tests =
  testGroup
    "Structured Outputs & Tools Property Tests"
    [ testProperty "StructuredResponse JSON roundtrip: ∀ x. decode (encode x) == Just x" $
        \(sr :: StructuredResponse SampleRecord) ->
          decode (encode sr) === Just sr
    , testProperty "formatFor produces SchemaFormat with properties" $
        case formatFor @SampleRecord of
          SchemaFormat s ->
            property
              ( not (Map.null (schemaProperties s))
                  && not (null (schemaRequired s))
              )
          JsonFormat -> property False
    , testProperty "toolResultMessageWithId correlation invariant" $
        \(c :: ModelName) (t :: ModelName) (cid :: ModelName) ->
          let content = unModelName c
              toolName = unModelName t
              callId = unModelName cid
              msg = toolResultMessageWithId content toolName callId
           in (messageRole msg === M.Tool)
                .&&. (messageContent msg === content)
                .&&. (messageToolName msg === Just toolName)
                .&&. (messageToolCallId msg === Just callId)
    , testProperty "toolResultMessage correlation invariant" $
        \(c :: ModelName) (t :: ModelName) ->
          let content = unModelName c
              toolName = unModelName t
              msg = toolResultMessage content toolName
           in (messageRole msg === M.Tool)
                .&&. (messageContent msg === content)
                .&&. (messageToolName msg === Just toolName)
                .&&. (messageToolCallId msg === Nothing)
    , testProperty "defaultToolLoopConfig positive max turns invariant" $
        property (tlcMaxTurns defaultToolLoopConfig > 0)
    ]
