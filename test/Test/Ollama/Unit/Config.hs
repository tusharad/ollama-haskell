module Test.Ollama.Unit.Config (tests) where

import Ollama.Client.Config
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Unit Config Tests"
    [ testCase "defaultConfig settings" $ do
        assertEqual "Default base URL" "http://127.0.0.1:11434" (configBaseUrl defaultConfig)
        assertEqual "Default timeout" 300 (configTimeout defaultConfig)
        assertEqual "Default retry policy" NoRetry (configRetry defaultConfig)
    , testCase "RetryPolicy smart constructors" $ do
        assertEqual "noRetry" NoRetry noRetry
        assertEqual "constantRetry" (ConstantRetry 3 1000000) (constantRetry 3 1000000)
        assertEqual "exponentialRetry" (ExponentialRetry 5 500000) (exponentialRetry 5 500000)
    ]
