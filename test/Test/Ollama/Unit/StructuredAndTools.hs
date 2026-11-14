module Test.Ollama.Unit.StructuredAndTools (tests) where

import Data.Aeson (FromJSON, ToJSON, Value (..), encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Ollama
import Ollama.Testing
import Ollama.Types.Message qualified as M
import Test.Tasty
import Test.Tasty.HUnit

data Person = Person
  { personName :: !Text
  , personAge :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data TaskItem = TaskItem
  { taskTitle :: !Text
  , taskPriority :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

tests :: TestTree
tests =
  testGroup
    "Unit Structured Outputs & Tool Calling Loop Tests"
    [ testCase "chatStructured parses valid JSON into typed StructuredResponse" $ do
        let rawMsg = "{\"personName\":\"Alice\",\"personAge\":30}"
            mockResp = mockChatResponse "llama3.2" rawMsg
        client <- newMockClient (BSL.toStrict (encode mockResp))
        let req = chatRequest "llama3.2" (userMessage "Generate person" :| [])
        res <- chatStructured @Person client req
        case res of
          Left err -> assertFailure $ "Expected Right, got: " <> show err
          Right sr -> do
            assertEqual "Parsed value matches" (Person "Alice" 30) (srValue sr)
            assertEqual "Model matches" "llama3.2" (crModel (srRawResponse sr))
    , testCase "chatStructured injects schema format into request wire bytes" $ do
        (client, getReqBytes) <- newMockClientRecording "{}"
        let req = chatRequest "llama3.2" (userMessage "Generate person" :| [])
        _ <- chatStructured @Person client req
        reqBytes <- getReqBytes
        assertBool "Contains format object" ("\"format\":" `BS.isInfixOf` reqBytes)
        assertBool "Contains schema type" ("\"type\":\"object\"" `BS.isInfixOf` reqBytes)
        assertBool "Contains personName property" ("\"personName\"" `BS.isInfixOf` reqBytes)
        assertBool "Contains personAge property" ("\"personAge\"" `BS.isInfixOf` reqBytes)
    , testCase "chatStructured returns DecodeError on malformed JSON payload" $ do
        let rawMsg = "{\"personName\": 123, broken json"
            mockResp = mockChatResponse "llama3.2" rawMsg
        client <- newMockClient (BSL.toStrict (encode mockResp))
        let req = chatRequest "llama3.2" (userMessage "Generate person" :| [])
        res <- chatStructured @Person client req
        case res of
          Left (DecodeError _ _) -> pure ()
          other -> assertFailure $ "Expected DecodeError, got: " <> show other
    , testCase "generateStructured parses valid JSON into tuple" $ do
        let rawMsg = "{\"taskTitle\":\"Write tests\",\"taskPriority\":1}"
            mockResp = mockGenerateResponse "llama3.2" rawMsg
        client <- newMockClient (BSL.toStrict (encode mockResp))
        let req = generateRequest "llama3.2" "Generate task"
        res <- generateStructured @TaskItem client req
        case res of
          Left err -> assertFailure $ "Expected Right, got: " <> show err
          Right (item, raw) -> do
            assertEqual "Parsed item matches" (TaskItem "Write tests" 1) item
            assertEqual "Raw model matches" "llama3.2" (grModel raw)
    , testCase "generateStructured injects schema format into request wire bytes" $ do
        (client, getReqBytes) <- newMockClientRecording "{}"
        let req = generateRequest "llama3.2" "Generate task"
        _ <- generateStructured @TaskItem client req
        reqBytes <- getReqBytes
        assertBool "Contains format in generate request" ("\"format\":" `BS.isInfixOf` reqBytes)
        assertBool "Contains taskTitle" ("\"taskTitle\"" `BS.isInfixOf` reqBytes)
    , testCase "generateStructured returns DecodeError on invalid schema match" $ do
        let rawMsg = "{\"unknown_field\": true}"
            mockResp = mockGenerateResponse "llama3.2" rawMsg
        client <- newMockClient (BSL.toStrict (encode mockResp))
        let req = generateRequest "llama3.2" "Generate task"
        res <- generateStructured @TaskItem client req
        case res of
          Left (DecodeError _ _) -> pure ()
          other -> assertFailure $ "Expected DecodeError, got: " <> show other
    , testCase "chatWithTools executes single tool call and reaches final answer" $ do
        let tc =
              toolCallWithId "call-1" (mkToolCallFunction "add" (Map.fromList [("a", Number 2), ("b", Number 3)]))
            turn1Msg = (assistantMessage "") {messageToolCalls = Just [tc]}
            turn1Resp = (mockChatResponse "qwen2.5" "") {crMessage = Just turn1Msg}
            turn2Msg = assistantMessage "The sum of 2 and 3 is 5."
            turn2Resp = (mockChatResponse "qwen2.5" "The sum of 2 and 3 is 5.") {crMessage = Just turn2Msg}
        client <- newMockClientSequence [BSL.toStrict (encode turn1Resp), BSL.toStrict (encode turn2Resp)]
        let addTool =
              RegisteredTool
                { rtTool =
                    toolDef
                      "add"
                      "Add two numbers"
                      (objectParams [("a", numberParam Nothing), ("b", numberParam Nothing)] ["a", "b"])
                , rtHandler = \args -> case (Map.lookup "a" args, Map.lookup "b" args) of
                    (Just (Number a), Just (Number b)) -> pure $ Right (Number (a + b))
                    _ -> pure $ Left "Invalid arguments"
                }
            req = chatRequest "qwen2.5" (userMessage "What is 2 + 3?" :| [])
        res <- chatWithTools client req [addTool] defaultToolLoopConfig
        case res of
          Left err -> assertFailure $ "chatWithTools failed: " <> show err
          Right (finalMsg, fullHistory) -> do
            assertEqual "Final answer matches" "The sum of 2 and 3 is 5." (messageContent finalMsg)
            assertEqual
              "Total messages: user + assistant_call + tool_result + final_assistant"
              4
              (length fullHistory)
            let toolResult = fullHistory !! 2
            assertEqual "Tool result role is Tool" M.Tool (messageRole toolResult)
            assertEqual "Tool result content is 5" "5" (messageContent toolResult)
            assertEqual "Tool call id correlated" (Just "call-1") (messageToolCallId toolResult)
    , testCase "chatWithTools executes multiple parallel tool calls in one turn" $ do
        let tc1 =
              toolCallWithId
                "call-1"
                (mkToolCallFunction "city_weather" (Map.fromList [("city", String "Paris")]))
            tc2 =
              toolCallWithId
                "call-2"
                (mkToolCallFunction "city_weather" (Map.fromList [("city", String "London")]))
            turn1Msg = (assistantMessage "") {messageToolCalls = Just [tc1, tc2]}
            turn1Resp = (mockChatResponse "qwen2.5" "") {crMessage = Just turn1Msg}
            turn2Msg = assistantMessage "Paris is 20C and London is 15C."
            turn2Resp = (mockChatResponse "qwen2.5" "Paris is 20C and London is 15C.") {crMessage = Just turn2Msg}
        client <- newMockClientSequence [BSL.toStrict (encode turn1Resp), BSL.toStrict (encode turn2Resp)]
        let weatherTool =
              RegisteredTool
                { rtTool =
                    toolDef "city_weather" "Get temperature" (objectParams [("city", stringParam Nothing)] ["city"])
                , rtHandler = \args -> case Map.lookup "city" args of
                    Just (String "Paris") -> pure $ Right (String "20C sunny")
                    Just (String "London") -> pure $ Right (String "15C rainy")
                    _ -> pure $ Left "Unknown city"
                }
            req = chatRequest "qwen2.5" (userMessage "Weather in Paris and London?" :| [])
        res <- chatWithTools client req [weatherTool] defaultToolLoopConfig
        case res of
          Left err -> assertFailure $ "chatWithTools parallel failed: " <> show err
          Right (finalMsg, fullHistory) -> do
            assertEqual "Final answer content" "Paris is 20C and London is 15C." (messageContent finalMsg)
            -- 1 (user) + 1 (assistant 2 tools) + 2 (tool results) + 1 (final assistant) = 5
            assertEqual "Total messages is 5" 5 (length fullHistory)
            let resParis = fullHistory !! 2
                resLondon = fullHistory !! 3
            assertEqual "Paris id" (Just "call-1") (messageToolCallId resParis)
            assertEqual "London id" (Just "call-2") (messageToolCallId resLondon)
    , testCase "chatWithTools formats tool errors gracefully" $ do
        let tc =
              toolCallWithId
                "call-err"
                (mkToolCallFunction "divide" (Map.fromList [("a", Number 10), ("b", Number 0)]))
            turn1Msg = (assistantMessage "") {messageToolCalls = Just [tc]}
            turn1Resp = (mockChatResponse "qwen2.5" "") {crMessage = Just turn1Msg}
            turn2Msg = assistantMessage "Error handled: division by zero"
            turn2Resp = (mockChatResponse "qwen2.5" "Error handled") {crMessage = Just turn2Msg}
        client <- newMockClientSequence [BSL.toStrict (encode turn1Resp), BSL.toStrict (encode turn2Resp)]
        let divTool =
              RegisteredTool
                { rtTool = toolDef "divide" "Division" (objectParams [] [])
                , rtHandler = \_ -> pure $ Left "division by zero"
                }
            req = chatRequest "qwen2.5" (userMessage "Divide 10 by 0" :| [])
        res <- chatWithTools client req [divTool] defaultToolLoopConfig
        case res of
          Left err -> assertFailure $ "chatWithTools error handling failed: " <> show err
          Right (finalMsg, fullHistory) -> do
            assertEqual "Final answer" "Error handled: division by zero" (messageContent finalMsg)
            let toolRes = fullHistory !! 2
            assertEqual "Tool error prefixed" "Error: division by zero" (messageContent toolRes)
    , testCase "chatWithTools handles unregistered tool gracefully" $ do
        let tc = toolCallWithId "call-unk" (mkToolCallFunction "unknown_calc" Map.empty)
            turn1Msg = (assistantMessage "") {messageToolCalls = Just [tc]}
            turn1Resp = (mockChatResponse "qwen2.5" "") {crMessage = Just turn1Msg}
            turn2Msg = assistantMessage "Tool was not available"
            turn2Resp = (mockChatResponse "qwen2.5" "") {crMessage = Just turn2Msg}
        client <- newMockClientSequence [BSL.toStrict (encode turn1Resp), BSL.toStrict (encode turn2Resp)]
        let req = chatRequest "qwen2.5" (userMessage "Use unknown calc" :| [])
        res <- chatWithTools client req [] defaultToolLoopConfig
        case res of
          Left err -> assertFailure $ "chatWithTools unregistered tool failed: " <> show err
          Right (_, fullHistory) -> do
            let toolRes = fullHistory !! 2
            assertBool "Mentions tool not available" ("is not available" `T.isInfixOf` messageContent toolRes)
    , testCase "chatWithTools enforces max turns recursion limit" $ do
        let tc = toolCallWithId "call-inf" (mkToolCallFunction "ping" Map.empty)
            turnMsg = (assistantMessage "") {messageToolCalls = Just [tc]}
            turnResp = (mockChatResponse "qwen2.5" "") {crMessage = Just turnMsg}
            -- 5 turns of tool calls
            fiveResps = replicate 5 (BSL.toStrict (encode turnResp))
        client <- newMockClientSequence fiveResps
        let pingTool =
              RegisteredTool
                { rtTool = toolDef "ping" "Ping" (objectParams [] [])
                , rtHandler = \_ -> pure $ Right (String "pong")
                }
            req = chatRequest "qwen2.5" (userMessage "Start ping" :| [])
            cfg = defaultToolLoopConfig {tlcMaxTurns = 3}
        res <- chatWithTools client req [pingTool] cfg
        case res of
          Left (InvalidRequest msg) ->
            assertBool "Reports maximum turns exceeded" ("Maximum tool loop turns" `T.isInfixOf` msg)
          other -> assertFailure $ "Expected Left InvalidRequest, got: " <> show other
    , testCase "chatWithTools triggers onTurn callback" $ do
        turnsRef <- newIORef ([] :: [Int])
        let tc = toolCallWithId "call-1" (mkToolCallFunction "echo" (Map.fromList [("x", String "hi")]))
            turn1Msg = (assistantMessage "") {messageToolCalls = Just [tc]}
            turn1Resp = (mockChatResponse "qwen2.5" "") {crMessage = Just turn1Msg}
            turn2Msg = assistantMessage "Done"
            turn2Resp = (mockChatResponse "qwen2.5" "Done") {crMessage = Just turn2Msg}
        client <- newMockClientSequence [BSL.toStrict (encode turn1Resp), BSL.toStrict (encode turn2Resp)]
        let echoTool =
              RegisteredTool
                { rtTool = toolDef "echo" "Echo" (objectParams [] [])
                , rtHandler = \_ -> pure $ Right (String "hi")
                }
            req = chatRequest "qwen2.5" (userMessage "Echo" :| [])
            cfg =
              defaultToolLoopConfig
                { tlcOnTurn = Just $ \turn _msg -> modifyIORef' turnsRef (++ [turn])
                }
        _ <- chatWithTools client req [echoTool] cfg
        recordedTurns <- readIORef turnsRef
        assertEqual "Callback was invoked for turn 1 and turn 2" [1, 2] recordedTurns
    ]
