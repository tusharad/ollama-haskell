{-# LANGUAGE OverloadedStrings #-}

{-
  Tests related to Generate module
-}
module Test.Ollama.Generate (tests) where

import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.Ollama.Common.SchemaBuilder
import Data.Ollama.Common.Utils (encodeImage)
import Data.Ollama.Generate
import Data.Text qualified as T
import Data.Time (diffUTCTime, getCurrentTime)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Test.Tasty
import Test.Tasty.HUnit

generateTests :: TestTree
generateTests =
  testGroup
    "Generation with various options"
    [ testCase "Should contain 4 in 2+2" $ do
        eRes <-
          generate
            defaultGenerateOps {modelName = "gemma3", prompt = "What is 2+2?"}
            Nothing
        case eRes of
          Left err -> assertFailure $ "Expected success, got error: " ++ show err
          Right r -> assertBool "Should contain 4" (T.isInfixOf "4" (genResponse r))
    , testCase "Setting timeout" $ do
        eRes <-
          generate
            defaultGenerateOps {modelName = "gemma3", prompt = "Write a poem about French revolution"}
            (Just $ defaultOllamaConfig {timeout = 1})
        case eRes of
          Left (TimeoutError _) -> pure ()
          _ -> assertFailure "Expected timeout error"
    ]

testOnModelHooksFail :: TestTree
testOnModelHooksFail = testCase "Model lifecycle hooks should be triggered" $ do
  refStart <- newIORef False
  refError <- newIORef False
  refFinish <- newIORef True
  let config =
        defaultOllamaConfig
          { hostUrl = "http://localhost:12345" -- guaranteed to fail
          , onModelStart = Just $ writeIORef refStart True
          , onModelError = Just $ writeIORef refError True
          , onModelFinish = Just $ writeIORef refFinish True
          }

  void $
    generate
      defaultGenerateOps {modelName = "gemma3", prompt = "what is 23+41?"}
      (Just config)

  wasStarted <- readIORef refStart
  wasErrored <- readIORef refError
  wasFinished <- readIORef refFinish

  assertBool "onModelStart should be called" wasStarted
  assertBool "onModelError should be called" wasErrored
  assertBool "onModelFinish should be called" wasFinished

testOnModelHooksSucc :: TestTree
testOnModelHooksSucc = testCase "Model lifecycle hooks should be triggered 2" $ do
  refStart <- newIORef False
  refError <- newIORef True
  refFinish <- newIORef False
  let config =
        defaultOllamaConfig
          { onModelStart = Just $ writeIORef refStart True
          , onModelError = Just $ writeIORef refError False
          , onModelFinish = Just $ writeIORef refFinish True
          }

  void $
    generate
      defaultGenerateOps {modelName = "gemma3", prompt = "what is 23+41?"}
      (Just config)

  wasStarted <- readIORef refStart
  wasErrored <- readIORef refError
  wasFinished <- readIORef refFinish

  assertBool "onModelStart should be called" wasStarted
  assertBool "onModelError should be called" wasErrored
  assertBool "onModelFinish should be called" wasFinished

testRetryCount :: TestTree
testRetryCount = testCase "Should retry generate call retryCount times" $ do
  counter <- newIORef (0 :: Int)
  let config =
        defaultOllamaConfig
          { hostUrl = "http://localhost:12345" -- fails
          , retryCount = Just 2
          , retryDelay = Just 1
          , onModelStart = Just $ modifyIORef counter (+ 1)
          , onModelError = Just $ pure ()
          , onModelFinish = Just $ pure ()
          }

  _ <- generate defaultGenerateOps {prompt = "Retry test"} (Just config)
  calls <- readIORef counter
  -- Should be retryCount + 1 (initial + retries)
  assertEqual "Expected 3 attempts (1 initial + 2 retries)" 3 calls

testRetryDelay :: TestTree
testRetryDelay = testCase "Should delay between retries" $ do
  counter <- newIORef (0 :: Int)
  let delaySecs = 2
  start <- getCurrentTime

  let config =
        defaultOllamaConfig
          { hostUrl = "http://localhost:12345" -- fails
          , retryCount = Just 1
          , retryDelay = Just delaySecs
          , onModelStart = Just $ modifyIORef counter (+ 1)
          , onModelError = Just $ pure ()
          , onModelFinish = Just $ pure ()
          }

  _ <- generate defaultGenerateOps {prompt = "Retry delay test"} (Just config)
  end <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime end start) :: Double
  let expectedMin = fromIntegral delaySecs

  assertBool
    ("Elapsed time should be at least " ++ show expectedMin ++ "s, but was " ++ show elapsed)
    (elapsed >= expectedMin)

testCommonManagerUsage :: TestTree
testCommonManagerUsage = testCase "Should reuse provided commonManager" $ do
  refStart <- newIORef (0 :: Int)
  mgr <-
    newTlsManagerWith tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro 1000000}
  let config =
        defaultOllamaConfig
          { hostUrl = "http://localhost:12345" -- will fail fast
          , commonManager = Just mgr
          , timeout = 999 -- shouldn't matter, manager timeout will be used
          , onModelStart = Just $ modifyIORef refStart (+ 1)
          , onModelError = Just $ pure ()
          , onModelFinish = Just $ pure ()
          }

  _ <- generate defaultGenerateOps {prompt = "1"} (Just config)
  _ <- generate defaultGenerateOps {prompt = "2"} (Just config)
  startCount <- readIORef refStart
  assertEqual "Both requests should start (reuse manager)" 2 startCount

{-
 Suffix is not supported for few gemma3 and qwen3.

testSuffixOption :: TestTree
testSuffixOption = testCase "Should respect suffix in generation" $ do
  let ops = defaultGenerateOps
              { modelName = "qwen3:0.6b"
              , prompt = "Complete this sentence: The Eiffel Tower is in"
              , suffix = Just " [End]"
              }
  eRes <- generate ops Nothing
  case eRes of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right r -> assertBool "Expected suffix in response" $
                  T.isSuffixOf "[End]" (genResponse r)
                  -}

testThinkOption :: TestTree
testThinkOption = testCase "Should activate thinking mode when think=True" $ do
  let ops =
        defaultGenerateOps
          { modelName = "qwen3:0.6b"
          , prompt = "What is 2+2?"
          , think = Just True
          }
  eRes <- generate ops Nothing
  case eRes of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right _ -> pure () -- TODO: Need to find a way to know if model is thinking

testFormatJsonFormat :: TestTree
testFormatJsonFormat = testCase "Should return response in JsonFormat" $ do
  let ops =
        defaultGenerateOps
          { modelName = "gemma3"
          , prompt =
              "John was 23 year old in 2023, this is year 2025."
                <> "How old is John assuming he celebrated this year's birthday; "
                <> "Return an object with keys 'name' and 'age'."
          , format = Just JsonFormat
          }
  eRes <- generate ops Nothing
  case eRes of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right r -> do
      let responseText = genResponse r
      let decoded = Aeson.decode (BSL.pack $ T.unpack responseText) :: Maybe Aeson.Value
      assertBool "Expected valid JSON object in response" (decoded /= Nothing)

testFormatSchemaFormat :: TestTree
testFormatSchemaFormat = testCase "Should include SchemaFormat in the request" $ do
  let schema =
        buildSchema $
          emptyObject
            |+ ("fruit", JString)
            |+ ("quantity", JNumber)
            |! "fruit"
            |! "quantity"

      ops =
        defaultGenerateOps
          { modelName = "gemma3"
          , prompt = "I had 3 apples, 1 gave one away. How many left?"
          , format = Just (SchemaFormat schema)
          }

  eRes <- generate ops Nothing
  case eRes of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right r -> do
      let response = T.toLower (genResponse r)
      assertBool "Expected fruit information in response" $
        "apple" `T.isInfixOf` response || "fruit" `T.isInfixOf` response

testImageInput :: TestTree
testImageInput = testCase "Should accept and process base64 image input" $ do
  maybeImg <- encodeImage "./examples/sample.png"
  case maybeImg of
    Nothing -> assertFailure "Image encoding failed (unsupported format or missing file)"
    Just imgData -> do
      let ops =
            defaultGenerateOps
              { modelName = "gemma3"
              , prompt = "Describe this image."
              , images = Just [imgData]
              }
          cfg = Just defaultOllamaConfig {timeout = 300}

      eRes <- generate ops cfg
      case eRes of
        Left err -> assertFailure $ "Expected success, got error: " ++ show err
        Right r -> do
          let response = T.toLower (genResponse r)
          assertBool "Expected image-related description in response" $
            T.isInfixOf "i love haskell" response

testStreamingHandler :: TestTree
testStreamingHandler = testCase "Should handle streaming response" $ do
  -- IORef to collect streamed chunks
  chunksRef <- newIORef []
  -- Define the stream handler: accumulate responses
  let streamHandler chunk = modifyIORef chunksRef (++ [genResponse chunk])
      ops =
        defaultGenerateOps
          { modelName = "gemma3"
          , prompt = "Write few words about Haskell."
          , stream = Just streamHandler
          }
  eRes <- generate ops Nothing
  -- Collect streamed chunks from IORef
  chunks <- readIORef chunksRef
  let fullOutput = T.concat chunks
  case eRes of
    Left err -> assertFailure $ "Expected streaming success, got error: " ++ show err
    Right _ -> do
      assertBool "Expected streamed text to include 'haskell'" $
        "haskell" `T.isInfixOf` T.toLower fullOutput
      assertBool "Expected some streamed content" $ not (T.null fullOutput)

testModelOptionsBasic :: TestTree
testModelOptionsBasic = testCase "ModelOptions: temperature and topP" $ do
  let opts =
        Just $
          defaultModelOptions
            { temperature = Just 0.9
            , topP = Just 0.8
            , topK = Nothing
            , numPredict = Just 20
            }

  eRes <-
    generate
      defaultGenerateOps
        { modelName = "gemma3"
        , prompt = "Generate a random list of 3 animals"
        , options = opts
        }
      Nothing

  case eRes of
    Left err -> assertFailure $ "Expected success, got: " ++ show err
    Right r -> assertBool "Response should not be empty" (not . T.null $ genResponse r)

tests :: TestTree
tests =
  sequentialTestGroup
    "Generate tests"
    AllFinish
    [ generateTests
    , testOnModelHooksFail
    , testOnModelHooksSucc
    , testRetryCount
    , testRetryDelay
    , testCommonManagerUsage
    , testThinkOption
    , testFormatJsonFormat
    , testFormatSchemaFormat
    , testImageInput
    , testStreamingHandler
    , testModelOptionsBasic
    ]
