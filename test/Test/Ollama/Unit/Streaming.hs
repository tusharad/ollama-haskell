module Test.Ollama.Unit.Streaming (tests) where

import Conduit (yield)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Time (UTCTime (..))
import Ollama
import Test.Tasty
import Test.Tasty.HUnit

fixedTime :: UTCTime
fixedTime = UTCTime (toEnum 59000) 0

tests :: TestTree
tests =
  testGroup
    "Unit Streaming Tests"
    [ testCase "decodeStreamLine on server error JSON raises ApiError 500" $ do
        let errJson = "{\"error\":\"model 'nonexistent' not found\"}"
            res = decodeStreamLine @ChatResponse errJson
        assertEqual
          "Decodes to ApiError 500"
          (Left (ApiError 500 "model 'nonexistent' not found"))
          res
    , testCase "decodeStreamLine on malformed JSON raises DecodeError" $ do
        let malformed = "{\"broken\": json"
            res = decodeStreamLine @ChatResponse malformed
        case res of
          Left (DecodeError _ _) -> pure ()
          other -> assertFailure $ "Expected DecodeError, got: " <> show other
    , testCase "decodeStreamLine on empty line yields Nothing" $ do
        let res = decodeStreamLine @ChatResponse ""
        assertEqual "Empty yields Nothing" (Right Nothing) res
    , testCase "decodeStreamLine on whitespace line yields Nothing" $ do
        let res = decodeStreamLine @ChatResponse "   \r\n\t  "
        assertEqual "Whitespace yields Nothing" (Right Nothing) res
    , testCase "decodeStreamLine on valid ChatResponse JSON chunk yields Just resp" $ do
        let chunk =
              ChatResponse
                { crModel = "llama3.2"
                , crCreatedAt = fixedTime
                , crMessage = Just (assistantMessage "Hello")
                , crDone = False
                , crDoneReason = Nothing
                , crTotalDuration = Nothing
                , crLoadDuration = Nothing
                , crPromptEvalCount = Nothing
                , crPromptEvalDuration = Nothing
                , crEvalCount = Nothing
                , crEvalDuration = Nothing
                , crLogprobs = Nothing
                }
            chunkJson = BSL.toStrict (encode chunk)
            res = decodeStreamLine @ChatResponse chunkJson
        assertEqual "Decodes valid chunk" (Right (Just chunk)) res
    , testCase "accumulateChatChunks combines streaming text tokens in order" $ do
        let c1 =
              ChatResponse
                { crModel = "llama3.2"
                , crCreatedAt = fixedTime
                , crMessage = Just (assistantMessage "The ")
                , crDone = False
                , crDoneReason = Nothing
                , crTotalDuration = Nothing
                , crLoadDuration = Nothing
                , crPromptEvalCount = Nothing
                , crPromptEvalDuration = Nothing
                , crEvalCount = Nothing
                , crEvalDuration = Nothing
                , crLogprobs = Nothing
                }
            c2 = c1 {crMessage = Just (assistantMessage "sky is ")}
            c3 =
              c1
                { crMessage = Just (assistantMessage "blue.")
                , crDone = True
                , crDoneReason = Just "stop"
                , crTotalDuration = Just (Duration 500000000)
                , crEvalCount = Just 4
                }
        case accumulateChatChunks [c1, c2, c3] of
          Left err -> assertFailure $ "Failed to accumulate chat chunks: " <> show err
          Right acc -> do
            assertEqual "Model matches" "llama3.2" (crModel acc)
            assertBool "Done is True" (crDone acc)
            assertEqual "Done reason matches" (Just "stop") (crDoneReason acc)
            assertEqual "Total duration captured" (Just (Duration 500000000)) (crTotalDuration acc)
            assertEqual "Eval count captured" (Just 4) (crEvalCount acc)
            case crMessage acc of
              Nothing -> assertFailure "Accumulated message is Nothing"
              Just msg -> do
                assertEqual "Role is Assistant" Assistant (messageRole msg)
                assertEqual "Content is concatenated" "The sky is blue." (messageContent msg)
    , testCase "accumulateChatChunks combines thinking tokens and tool calls" $ do
        let tc1 = toolCallWithId "call-1" (ToolCallFunction "get_weather" mempty)
            c1 =
              ChatResponse
                { crModel = "deepseek-r1"
                , crCreatedAt = fixedTime
                , crMessage =
                    Just $
                      (assistantMessage "")
                        { messageThinking = Just "Thinking step 1... "
                        }
                , crDone = False
                , crDoneReason = Nothing
                , crTotalDuration = Nothing
                , crLoadDuration = Nothing
                , crPromptEvalCount = Nothing
                , crPromptEvalDuration = Nothing
                , crEvalCount = Nothing
                , crEvalDuration = Nothing
                , crLogprobs = Nothing
                }
            c2 =
              ChatResponse
                { crModel = "deepseek-r1"
                , crCreatedAt = fixedTime
                , crMessage =
                    Just $
                      (assistantMessage "Result")
                        { messageThinking = Just "Step 2 done."
                        , messageToolCalls = Just [tc1]
                        }
                , crDone = True
                , crDoneReason = Just "tool_calls"
                , crTotalDuration = Nothing
                , crLoadDuration = Nothing
                , crPromptEvalCount = Nothing
                , crPromptEvalDuration = Nothing
                , crEvalCount = Nothing
                , crEvalDuration = Nothing
                , crLogprobs = Nothing
                }
        case accumulateChatChunks [c1, c2] of
          Left err -> assertFailure $ "Failed to accumulate: " <> show err
          Right acc -> case crMessage acc of
            Nothing -> assertFailure "Expected Just message in accumulated ChatResponse"
            Just msg -> do
              assertEqual "Thinking concatenated" (Just "Thinking step 1... Step 2 done.") (messageThinking msg)
              assertEqual "Content preserved" "Result" (messageContent msg)
              assertEqual "Tool calls preserved" (Just [tc1]) (messageToolCalls msg)
    , testCase "accumulateChatChunks on empty list returns InvalidRequest" $ do
        case accumulateChatChunks [] of
          Left (InvalidRequest _) -> pure ()
          other -> assertFailure $ "Expected Left InvalidRequest, got: " <> show other
    , testCase "accumulateGenerateChunks combines response text and thinking" $ do
        let g1 =
              GenerateResponse
                { grModel = "qwen2.5"
                , grCreatedAt = fixedTime
                , grResponse = "Hello "
                , grDone = False
                , grDoneReason = Nothing
                , grContext = Nothing
                , grTotalDuration = Nothing
                , grLoadDuration = Nothing
                , grPromptEvalCount = Nothing
                , grPromptEvalDuration = Nothing
                , grEvalCount = Nothing
                , grEvalDuration = Nothing
                , grThinking = Just "thinking 1. "
                , grImage = Nothing
                , grLogprobs = Nothing
                , grCompleted = Nothing
                , grTotal = Nothing
                }
            g2 =
              g1
                { grResponse = "world!"
                , grThinking = Just "thinking 2."
                , grDone = True
                , grDoneReason = Just "stop"
                , grTotalDuration = Just (Duration 100000000)
                , grContext = Just [1, 2, 3]
                }
        case accumulateGenerateChunks [g1, g2] of
          Left err -> assertFailure $ "Failed to accumulate generate chunks: " <> show err
          Right acc -> do
            assertEqual "Model matches" "qwen2.5" (grModel acc)
            assertEqual "Response concatenated" "Hello world!" (grResponse acc)
            assertEqual "Thinking concatenated" (Just "thinking 1. thinking 2.") (grThinking acc)
            assertBool "Done is True" (grDone acc)
            assertEqual "Context captured" (Just [1, 2, 3]) (grContext acc)
            assertEqual "Total duration captured" (Just (Duration 100000000)) (grTotalDuration acc)
    , testCase "accumulateGenerateChunks on empty list returns InvalidRequest" $ do
        case accumulateGenerateChunks [] of
          Left (InvalidRequest _) -> pure ()
          other -> assertFailure $ "Expected Left InvalidRequest, got: " <> show other
    , testCase "accumulateChatStream Conduit folds stream of chunks" $ do
        let c1 =
              ChatResponse
                { crModel = "llama3.2"
                , crCreatedAt = fixedTime
                , crMessage = Just (assistantMessage "Stream ")
                , crDone = False
                , crDoneReason = Nothing
                , crTotalDuration = Nothing
                , crLoadDuration = Nothing
                , crPromptEvalCount = Nothing
                , crPromptEvalDuration = Nothing
                , crEvalCount = Nothing
                , crEvalDuration = Nothing
                , crLogprobs = Nothing
                }
            c2 = c1 {crMessage = Just (assistantMessage "test!"), crDone = True}
            stream = do
              yield c1
              yield c2
        res <- accumulateChatStream stream
        case res of
          Left err -> assertFailure $ "accumulateChatStream failed: " <> show err
          Right acc -> do
            assertEqual "Content concatenated" "Stream test!" (maybe "" messageContent (crMessage acc))
            assertBool "Done is True" (crDone acc)
    , testCase "accumulateGenerateStream Conduit folds stream of chunks" $ do
        let g1 =
              GenerateResponse
                { grModel = "llama3.2"
                , grCreatedAt = fixedTime
                , grResponse = "Gen "
                , grDone = False
                , grDoneReason = Nothing
                , grContext = Nothing
                , grTotalDuration = Nothing
                , grLoadDuration = Nothing
                , grPromptEvalCount = Nothing
                , grPromptEvalDuration = Nothing
                , grEvalCount = Nothing
                , grEvalDuration = Nothing
                , grThinking = Nothing
                , grImage = Nothing
                , grLogprobs = Nothing
                , grCompleted = Nothing
                , grTotal = Nothing
                }
            g2 = g1 {grResponse = "stream!", grDone = True}
            stream = do
              yield g1
              yield g2
        res <- accumulateGenerateStream stream
        case res of
          Left err -> assertFailure $ "accumulateGenerateStream failed: " <> show err
          Right acc -> do
            assertEqual "Response concatenated" "Gen stream!" (grResponse acc)
            assertBool "Done is True" (grDone acc)
    ]
