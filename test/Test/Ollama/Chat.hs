{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Ollama.Chat (tests) where

import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (fromList, NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as HM
import Data.Maybe (isJust)
import Data.Ollama.Chat
import Data.Scientific
import Data.Text qualified as T
import Data.Time (diffUTCTime, getCurrentTime)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Test.Tasty
import Test.Tasty.HUnit

-- | Basic chat test with default options
basicChatTest :: TestTree
basicChatTest = testCase "Basic chat should contain 4 for 2+2" $ do
  let ops = defaultChatOps
  eRes <- chat ops Nothing
  case eRes of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right r -> case message r of
      Nothing -> assertFailure "Expected a message in response"
      Just msg -> assertBool "Should contain '4'" (T.isInfixOf "4" (content msg))

-- | Test timeout configuration
timeoutTest :: TestTree
timeoutTest = testCase "Setting timeout" $ do
  let config = Just $ defaultOllamaConfig {timeout = 1}
  eRes <- chat defaultChatOps { messages = userMessage "Write a poem about greek history" :| [] } config
  case eRes of
    Right _ -> assertFailure "The model responded before timeout"
    Left (TimeoutError _) -> pure ()
    Left other -> assertFailure $ "Expected timeout error, got " ++ show other

-- | Test model lifecycle hooks on failure
hooksFailTest :: TestTree
hooksFailTest = testCase "Model lifecycle hooks should trigger on failure" $ do
  refStart <- newIORef False
  refError <- newIORef False
  refFinish <- newIORef True
  let config =
        defaultOllamaConfig
          { hostUrl = "http://localhost:12345" -- Guaranteed to fail
          , onModelStart = Just $ writeIORef refStart True
          , onModelError = Just $ writeIORef refError True
          , onModelFinish = Just $ writeIORef refFinish False
          }
  void $ chat defaultChatOps (Just config)
  wasStarted <- readIORef refStart
  wasErrored <- readIORef refError
  wasFinished <- readIORef refFinish
  assertBool "onModelStart should be called" wasStarted
  assertBool "onModelError should be called" wasErrored
  assertBool "onModelFinish should be called" wasFinished

-- | Test model lifecycle hooks on success
hooksSuccessTest :: TestTree
hooksSuccessTest = testCase "Model lifecycle hooks should trigger on success" $ do
  refStart <- newIORef False
  refError <- newIORef True
  refFinish <- newIORef False
  let config =
        defaultOllamaConfig
          { onModelStart = Just $ writeIORef refStart True
          , onModelError = Just $ writeIORef refError False
          , onModelFinish = Just $ writeIORef refFinish True
          }
  void $ chat defaultChatOps (Just config)
  wasStarted <- readIORef refStart
  wasErrored <- readIORef refError
  wasFinished <- readIORef refFinish
  assertBool "onModelStart should be called" wasStarted
  assertBool "onModelError should not be called" wasErrored
  assertBool "onModelFinish should be called" wasFinished

-- | Test retry count
retryCountTest :: TestTree
retryCountTest = testCase "Should retry chat call retryCount times" $ do
  counter <- newIORef (0 :: Int)
  let config =
        defaultOllamaConfig
          { hostUrl = "http://localhost:12345" -- Fails
          , retryCount = Just 2
          , retryDelay = Just 1
          , onModelStart = Just $ modifyIORef counter (+ 1)
          , onModelError = Just $ pure ()
          , onModelFinish = Just $ pure ()
          }
  _ <- chat defaultChatOps (Just config)
  calls <- readIORef counter
  assertEqual "Expected 3 attempts (1 initial + 2 retries)" 3 calls

-- | Test retry delay
retryDelayTest :: TestTree
retryDelayTest = testCase "Should delay between retries" $ do
  counter <- newIORef (0 :: Int)
  let delaySecs = 2
  start <- getCurrentTime
  let config =
        defaultOllamaConfig
          { hostUrl = "http://localhost:12345" -- Fails
          , retryCount = Just 1
          , retryDelay = Just delaySecs
          , onModelStart = Just $ modifyIORef counter (+ 1)
          , onModelError = Just $ pure ()
          , onModelFinish = Just $ pure ()
          }
  _ <- chat defaultChatOps (Just config)
  end <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime end start) :: Double
      expectedMin = fromIntegral delaySecs
  assertBool
    ("Elapsed time should be at least " ++ show expectedMin ++ "s, but was " ++ show elapsed)
    (elapsed >= expectedMin)

-- | Test common manager usage
commonManagerTest :: TestTree
commonManagerTest = testCase "Should reuse provided commonManager" $ do
  refStart <- newIORef (0 :: Int)
  mgr <-
    newTlsManagerWith
      tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro 1000000}
  let config =
        defaultOllamaConfig
          { hostUrl = "http://localhost:12345" -- Will fail fast
          , commonManager = Just mgr
          , timeout = 999 -- Shouldn’t matter, manager timeout takes precedence
          , onModelStart = Just $ modifyIORef refStart (+ 1)
          , onModelError = Just $ pure ()
          , onModelFinish = Just $ pure ()
          }
  _ <- chat defaultChatOps (Just config)
  _ <- chat defaultChatOps (Just config)
  startCount <- readIORef refStart
  assertEqual "Both requests should start (reuse manager)" 2 startCount

-- | Test JSON format response
jsonFormatTest :: TestTree
jsonFormatTest = testCase "Should return response in JSON format" $ do
  let ops =
        defaultChatOps
          { messages =
              fromList
                [userMessage "Return a JSON with keys 'name' and 'age' for John, 25 years old."]
          , format = Just JsonFormat
          }
  eRes <- chat ops Nothing
  case eRes of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right r -> case message r of
      Nothing -> assertFailure "Expected a message in response"
      Just msg -> do
        let responseText = content msg
        let decoded = Aeson.decode (BSL.pack $ T.unpack responseText) :: Maybe Aeson.Value
        assertBool "Expected valid JSON object in response" (isJust decoded)

-- | Test streaming response
streamingTest :: TestTree
streamingTest = testCase "Should handle streaming response" $ do
  chunksRef <- newIORef []
  let streamHandler chunk = modifyIORef chunksRef (++ [message chunk])
      ops = defaultChatOps {stream = Just (streamHandler, pure ())}
  eRes <- chat ops Nothing
  chunks <- readIORef chunksRef
  let fullOutput = T.concat (map (maybe "" content) chunks)
  case eRes of
    Left err -> assertFailure $ "Expected streaming success, got error: " ++ show err
    Right _ -> assertBool "Expected some streamed content" (not $ T.null fullOutput)

-- | Test custom model options
modelOptionsTest :: TestTree
modelOptionsTest = testCase "Should use custom model options" $ do
  let opts =
        Just $
          defaultModelOptions
            { temperature = Just 0.9
            , topP = Just 0.8
            , topK = Nothing
            , numPredict = Just 20
            }
      ops = defaultChatOps {options = opts}
  eRes <- chat ops Nothing
  case eRes of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right r -> assertBool "Expected a response message" (isJust (message r))

testToolCallAddTwoNumbers :: TestTree
testToolCallAddTwoNumbers = testCase "Tool call: addTwoNumbers(23, 46)" $ do
  let messageList = NE.singleton $ userMessage "What is 23 + 46? (Use tool)"
      paramProps =
        HM.fromList
          [ ("a", FunctionParameters "number" Nothing Nothing Nothing)
          , ("b", FunctionParameters "number" Nothing Nothing Nothing)
          ]
      functionParams =
        FunctionParameters
          { parameterType = "object"
          , requiredParams = Just ["a", "b"]
          , parameterProperties = Just paramProps
          , additionalProperties = Just False
          }
      functionDef =
        FunctionDef
          { functionName = "addTwoNumbers"
          , functionDescription = Just "Add two numbers"
          , functionParameters = Just functionParams
          , functionStrict = Nothing
          }
      tool =
        InputTool
          { toolType = "function"
          , function = functionDef
          }
      ops =
        defaultChatOps
          { modelName = "qwen3.5:2b"
          , messages = messageList
          , tools = Just [tool]
          }

  res <- chat ops Nothing
  case res of
    Left err -> assertFailure $ "Chat failed: " ++ show err
    Right ChatResponse {message = Nothing} -> assertFailure "No message in response"
    Right ChatResponse {message = Just msg} ->
      case tool_calls msg of
        Nothing -> assertFailure "No tool calls received"
        Just [toolCall] -> do
          result <- captureAddToolCall toolCall
          assertEqual "Expected result of 23 + 46" 69 result
        Just other -> assertFailure $ "Unexpected number of tool calls: " ++ show other

-- Helper to evaluate the tool call
captureAddToolCall :: ToolCall -> IO Int
captureAddToolCall (ToolCall func)
  | outputFunctionName func == "addTwoNumbers" =
      case ( HM.lookup "a" (arguments func) >>= convertToNumber
           , HM.lookup "b" (arguments func) >>= convertToNumber
           ) of
        (Just a, Just b) -> return $ addTwoNumbers a b
        _ -> assertFailure "Missing parameters a or b" >> return 0
  | otherwise = assertFailure "Unexpected function name" >> return 0

addTwoNumbers :: Int -> Int -> Int
addTwoNumbers = (+)

-- Convert Aeson value to Int
convertToNumber :: Aeson.Value -> Maybe Int
convertToNumber (Aeson.Number n) = toBoundedInteger n
convertToNumber _ = Nothing

-- | Test conversation with multiple messages
multiMessageConversationTest :: TestTree
multiMessageConversationTest = testCase "Multi-message conversation should work" $ do
  let msgs =
        fromList
          [ systemMessage "You are a helpful assistant."
          , userMessage "What is 2+2?"
          , assistantMessage "2+2 equals 4."
          , userMessage "What about 3+3?"
          ]
      ops = defaultChatOps {modelName = "qwen3.5:2b", messages = msgs}
  eRes <- chat ops Nothing
  case eRes of
    Left _ -> assertFailure "Expected success, got error"
    Right r -> case message r of
      Nothing -> assertFailure "Expected a message in response"
      Just msg -> assertBool "Should contain '6'" (T.isInfixOf "6" (content msg))

-- | Test invalid model name
invalidModelChatTest :: TestTree
invalidModelChatTest = testCase "Invalid model name should fail" $ do
  let ops = defaultChatOps {modelName = "nonexistent-chat-model"}
  eRes <- chat ops Nothing
  case eRes of
    Left _ -> assertBool "Should fail with invalid model" True
    Right _ -> assertFailure "Should not succeed with invalid model"

-- | Group all tests
tests :: TestTree
tests =
  sequentialTestGroup
    "Chat tests"
    AllFinish
    [ basicChatTest
    , timeoutTest
    , hooksFailTest
    , hooksSuccessTest
    , retryCountTest
    , retryDelayTest
    , commonManagerTest
    , jsonFormatTest
    , streamingTest
    , modelOptionsTest
    , testToolCallAddTwoNumbers
    , multiMessageConversationTest
    , invalidModelChatTest
    ]
