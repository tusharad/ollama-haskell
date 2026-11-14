module Test.Ollama.Unit.WebSearch (tests) where

import Data.Aeson (decode, encode)
import Data.Maybe (isJust)
import Ollama
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Unit WebSearch & WebFetch Tests"
    [ testCase "WebSearchRequest serialization with max_results" $ do
        let req = WebSearchRequest "Haskell 2026" (Just 5)
            jsonBytes = encode req
            expected = "{\"max_results\":5,\"query\":\"Haskell 2026\"}"
        assertEqual "Serialized JSON match" expected jsonBytes
    , testCase "WebSearchRequest serialization without max_results" $ do
        let req = webSearchRequest "Haskell functional programming"
            jsonBytes = encode req
            expected = "{\"query\":\"Haskell functional programming\"}"
        assertEqual "Serialized JSON match" expected jsonBytes
    , testCase "WebSearchResponse roundtrip decode" $ do
        let rawJson =
              "{\"results\":[{\"title\":\"Haskell Org\",\"url\":\"https://www.haskell.org\",\"content\":\"An advanced purely functional programming language.\"}]}"
        case decode @WebSearchResponse rawJson of
          Nothing -> assertFailure "Failed to decode WebSearchResponse"
          Just resp ->
            case wsResults resp of
              [item] -> do
                assertEqual "Title match" "Haskell Org" (srTitle item)
                assertEqual "Url match" "https://www.haskell.org" (srUrl item)
                assertEqual "Content match" "An advanced purely functional programming language." (srContent item)
              other -> assertFailure $ "Expected 1 result, got: " <> show (length other)
    , testCase "WebFetchRequest serialization" $ do
        let req = webFetchRequest "https://ollama.com"
            jsonBytes = encode req
            expected = "{\"url\":\"https://ollama.com\"}"
        assertEqual "Serialized JSON match" expected jsonBytes
    , testCase "WebFetchResponse roundtrip decode" $ do
        let rawJson =
              "{\"title\":\"Ollama\",\"content\":\"Get up and running with large language models.\"}"
        case decode @WebFetchResponse rawJson of
          Nothing -> assertFailure "Failed to decode WebFetchResponse"
          Just resp -> do
            assertEqual "Title match" "Ollama" (wfrTitle resp)
            assertEqual "Content match" "Get up and running with large language models." (wfrContent resp)
    , testCase "webSearchTool produces valid Tool schema" $ do
        let tool = webSearchTool
        assertEqual "Tool type is function" "function" (toolType tool)
        let fn = toolFunction tool
        assertEqual "Function name is web_search" "web_search" (fnName fn)
        assertBool "Description present" (isJust (fnDescription fn))
        case fnParameters fn of
          Nothing -> assertFailure "Parameters missing"
          Just p -> do
            assertEqual "Parameter type object" "object" (fpType p)
            assertEqual "Required fields" (Just ["query"]) (fpRequired p)
    , testCase "webFetchTool produces valid Tool schema" $ do
        let tool = webFetchTool
        assertEqual "Tool type is function" "function" (toolType tool)
        let fn = toolFunction tool
        assertEqual "Function name is web_fetch" "web_fetch" (fnName fn)
        assertBool "Description present" (isJust (fnDescription fn))
        case fnParameters fn of
          Nothing -> assertFailure "Parameters missing"
          Just p -> do
            assertEqual "Parameter type object" "object" (fpType p)
            assertEqual "Required fields" (Just ["url"]) (fpRequired p)
    , testCase "webSearch fails with InvalidRequest if configApiKey is Nothing" $ do
        client <- defaultClient
        res <- webSearch client (webSearchRequest "test")
        case res of
          Left (InvalidRequest msg) ->
            assertEqual
              "API key error message"
              "Ollama API key required for web search. Set OLLAMA_API_KEY or configApiKey."
              msg
          other -> assertFailure $ "Expected Left InvalidRequest, got: " <> show other
    , testCase "webFetch fails with InvalidRequest if configApiKey is Nothing" $ do
        client <- defaultClient
        res <- webFetch client (webFetchRequest "https://example.com")
        case res of
          Left (InvalidRequest msg) ->
            assertEqual
              "API key error message"
              "Ollama API key required for web fetch. Set OLLAMA_API_KEY or configApiKey."
              msg
          other -> assertFailure $ "Expected Left InvalidRequest, got: " <> show other
    , testCase "webSearch with mock client and API key succeeds" $ do
        let mockPayload =
              "{\"results\":[{\"title\":\"Test Title\",\"url\":\"https://test.com\",\"content\":\"Test snippet\"}]}"
            cfg = defaultConfig {configBaseUrl = "http://mock-cloud", configApiKey = Just "test-key"}
        client <- newMockClientWithConfig cfg mockPayload
        res <- webSearch client (webSearchRequest "query")
        case res of
          Left err -> assertFailure $ "webSearch failed: " <> show err
          Right resp ->
            case wsResults resp of
              [item] -> assertEqual "Result title" "Test Title" (srTitle item)
              other -> assertFailure $ "Expected 1 result, got: " <> show (length other)
    , testCase "webFetch with mock client and API key succeeds" $ do
        let mockPayload =
              "{\"title\":\"Fetched Page\",\"content\":\"Page content text\"}"
            cfg = defaultConfig {configBaseUrl = "http://mock-cloud", configApiKey = Just "test-key"}
        client <- newMockClientWithConfig cfg mockPayload
        res <- webFetch client (webFetchRequest "https://test.com")
        case res of
          Left err -> assertFailure $ "webFetch failed: " <> show err
          Right resp -> do
            assertEqual "Title match" "Fetched Page" (wfrTitle resp)
            assertEqual "Content match" "Page content text" (wfrContent resp)
    ]
