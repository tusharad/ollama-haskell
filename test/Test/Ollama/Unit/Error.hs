module Test.Ollama.Unit.Error (tests) where

import Control.Exception (try)
import Ollama.Error
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Unit Error Tests"
    [ testCase "isRetryable correctly classifies transient vs non-transient errors" $ do
        assertBool "TimeoutError is retryable" (isRetryable TimeoutError)
        assertBool "ApiError is not retryable" (not $ isRetryable (ApiError 404 "Not Found"))
        assertBool "InvalidRequest is not retryable" (not $ isRetryable (InvalidRequest "Bad model"))
    , testCase "OllamaError Eq instance compares ApiError and TimeoutError" $ do
        assertEqual "ApiError equal" (ApiError 500 "Server Error") (ApiError 500 "Server Error")
        assertBool "ApiError not equal to TimeoutError" (ApiError 500 "Server Error" /= TimeoutError)
    , testCase "throwOllama raises OllamaError as an Exception" $ do
        res <- try (throwOllama TimeoutError) :: IO (Either OllamaError ())
        assertEqual "Caught thrown OllamaError" (Left TimeoutError) res
    ]
