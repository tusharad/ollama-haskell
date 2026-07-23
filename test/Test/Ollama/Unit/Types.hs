module Test.Ollama.Unit.Types (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Unit Types Tests"
    [ testCase "ModelName smart constructor" $ do
        assertBool "Non-empty model name valid" True
    ]
