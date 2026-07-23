module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "ollama-haskell Integration Test Suite"
    [ testCase "Placeholder integration test" $ do
        assertBool "Server reachable test" True
    ]

main :: IO ()
main = defaultMain tests
