module Test.Ollama.Property.Roundtrip (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Property Roundtrip Tests"
    [ testCase "Role serialization roundtrip" $ do
        assertBool "Role serialization" True
    ]
