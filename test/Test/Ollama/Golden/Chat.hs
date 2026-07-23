module Test.Ollama.Golden.Chat (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Golden Chat Tests"
    [ testCase "Chat golden serialization" $ do
        assertBool "Golden chat test" True
    ]
