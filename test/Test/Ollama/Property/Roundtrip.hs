module Test.Ollama.Property.Roundtrip (tests) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Ollama
import Test.Ollama.Property.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Property & Roundtrip QuickCheck Tests"
    [ testProperty "Role JSON roundtrip: ∀ x. decode (encode x) == Just x" $
        \(r :: Role) -> decode (encode r) === Just r
    , testProperty "Message JSON roundtrip: ∀ x. decode (encode x) == Just x" $
        \(msg :: Message) -> decode (encode msg) === Just msg
    , testProperty "ModelName JSON roundtrip: ∀ x. decode (encode x) == Just x" $
        \(m :: ModelName) -> decode (encode m) === Just m
    , testProperty "Think JSON roundtrip: ∀ x. decode (encode x) == Just x" $
        \(t :: Think) -> decode (encode t) === Just t
    , testProperty "Digest JSON roundtrip: ∀ x. decode (encode x) == Just x" $
        \(d :: Digest) -> decode (encode d) === Just d
    , testProperty "Duration JSON roundtrip: ∀ x. decode (encode x) == Just x" $
        \(dur :: Duration) -> decode (encode dur) === Just dur
    , testProperty "Idempotency property: ∀ x. encode (fromJust (decode (encode x))) == encode x" $
        \(msg :: Message) -> encode (fromJust (decode (encode msg) :: Maybe Message)) === encode msg
    , testProperty "Smart constructor invariant: mkModelName never returns empty ModelName" $
        \txt -> case mkModelName (T.pack txt) of
          Left err -> err === "Model name cannot be empty"
          Right (ModelName name) -> not (T.null name) === True
    ]
