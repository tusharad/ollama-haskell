module Test.Ollama.Unit.ModelLifecycle (tests) where

import Data.Aeson (encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Ollama
import Ollama.Testing
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Unit Model Lifecycle & GPU VRAM Management Tests"
    [ testCase "unloadModel sends non-streaming generate request with keep_alive 0" $ do
        (client, getReqBytes) <- newMockClientRecording "{}"
        res <- unloadModel client "llama3.2"
        assertEqual "Unload returns Right ()" (Right ()) res
        reqBytes <- getReqBytes
        assertBool "Contains keep_alive 0" ("\"keep_alive\":\"0\"" `BS.isInfixOf` reqBytes)
        assertBool "Contains empty prompt" ("\"prompt\":\"\"" `BS.isInfixOf` reqBytes)
        assertBool "Contains model name" ("\"model\":\"llama3.2\"" `BS.isInfixOf` reqBytes)
        assertBool "Contains stream false" ("\"stream\":false" `BS.isInfixOf` reqBytes)
    , testCase "unloadModel succeeds on empty response body" $ do
        (client, _) <- newMockClientRecording ""
        res <- unloadModel client "llama3.2"
        assertEqual "Unload returns Right () on empty response" (Right ()) res
    , testCase "preloadModel with Nothing defaults keep_alive to 5m" $ do
        (client, getReqBytes) <- newMockClientRecording "{}"
        res <- preloadModel client "qwen2.5" Nothing
        assertEqual "Preload returns Right ()" (Right ()) res
        reqBytes <- getReqBytes
        assertBool "Contains keep_alive 5m" ("\"keep_alive\":\"5m\"" `BS.isInfixOf` reqBytes)
        assertBool "Contains empty prompt" ("\"prompt\":\"\"" `BS.isInfixOf` reqBytes)
        assertBool "Contains model name" ("\"model\":\"qwen2.5\"" `BS.isInfixOf` reqBytes)
        assertBool "Contains stream false" ("\"stream\":false" `BS.isInfixOf` reqBytes)
    , testCase "preloadModel with Duration formats duration in seconds" $ do
        (client, getReqBytes) <- newMockClientRecording "{}"
        res <- preloadModel client "qwen2.5" (Just (Duration 300000000000))
        assertEqual "Preload returns Right ()" (Right ()) res
        reqBytes <- getReqBytes
        assertBool "Contains keep_alive 300s" ("\"keep_alive\":\"300s\"" `BS.isInfixOf` reqBytes)
    , testCase "preloadModelWith with custom string sends specified keep_alive" $ do
        (client, getReqBytes) <- newMockClientRecording "{}"
        res <- preloadModelWith client "qwen2.5" (Just "10m")
        assertEqual "PreloadWith returns Right ()" (Right ()) res
        reqBytes <- getReqBytes
        assertBool "Contains keep_alive 10m" ("\"keep_alive\":\"10m\"" `BS.isInfixOf` reqBytes)
    , testCase "preloadModel and unloadModel propagate server errors" $ do
        client <- newMockClientWithStatus 500 "{\"error\":\"server failed to load model\"}"
        res <- unloadModel client "llama3.2"
        case res of
          Left (ApiError 500 _) -> pure ()
          other -> assertFailure $ "Expected ApiError 500, got: " <> show other
    , testCase "isModelRunning returns True when model is present in running list" $ do
        let mockData = mockRunningModelsResponse [(ModelName "llama3.2:latest", 4000000000)]
        client <- newMockClient (BSL.toStrict (encode mockData))
        res <- isModelRunning client (ModelName "llama3.2:latest")
        assertEqual "Model is running" (Right True) res
    , testCase "isModelRunning handles tag normalization (query llama3.2 matches llama3.2:latest)" $ do
        let mockData = mockRunningModelsResponse [(ModelName "llama3.2:latest", 4000000000)]
        client <- newMockClient (BSL.toStrict (encode mockData))
        res <- isModelRunning client (ModelName "llama3.2")
        assertEqual "Model without tag matches :latest" (Right True) res
    , testCase "isModelRunning handles reverse tag normalization (query llama3.2:latest matches llama3.2)" $ do
        let mockData = mockRunningModelsResponse [(ModelName "llama3.2", 4000000000)]
        client <- newMockClient (BSL.toStrict (encode mockData))
        res <- isModelRunning client (ModelName "llama3.2:latest")
        assertEqual "Model with :latest matches untagged" (Right True) res
    , testCase "isModelRunning returns False when model is absent" $ do
        let mockData = mockRunningModelsResponse [(ModelName "mistral:latest", 4000000000)]
        client <- newMockClient (BSL.toStrict (encode mockData))
        res <- isModelRunning client (ModelName "llama3.2")
        assertEqual "Model is not running" (Right False) res
    , testCase "getModelVramUsage returns Just bytes when model is running" $ do
        let mockData = mockRunningModelsResponse [(ModelName "llama3.2", 4294967296)]
        client <- newMockClient (BSL.toStrict (encode mockData))
        res <- getModelVramUsage client (ModelName "llama3.2")
        assertEqual "VRAM usage is Just 4GB" (Right (Just 4294967296)) res
    , testCase "getModelVramUsage returns Nothing when model is not running" $ do
        let mockData = mockRunningModelsResponse [(ModelName "mistral", 4294967296)]
        client <- newMockClient (BSL.toStrict (encode mockData))
        res <- getModelVramUsage client (ModelName "llama3.2")
        assertEqual "VRAM usage is Nothing" (Right Nothing) res
    , testCase "isModelRunning and getModelVramUsage propagate server errors" $ do
        client1 <- newMockClientWithStatus 500 "server ps failure"
        resPs <- isModelRunning client1 (ModelName "llama3.2")
        assertEqual "isModelRunning propagates 500" (Left (ApiError 500 "server ps failure")) resPs
        client2 <- newMockClientWithStatus 500 "server ps failure"
        resVram <- getModelVramUsage client2 (ModelName "llama3.2")
        assertEqual "getModelVramUsage propagates 500" (Left (ApiError 500 "server ps failure")) resVram
    ]

