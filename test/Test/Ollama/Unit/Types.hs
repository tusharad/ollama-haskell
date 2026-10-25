module Test.Ollama.Unit.Types (tests) where

import Conduit (yield)
import Data.Aeson (decode, encode)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Time (getCurrentTime)
import Ollama
import Ollama.Types.Message qualified as M
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Unit Types & API Tests"
    [ testCase "ModelName smart constructor validation" $ do
        assertEqual "Empty name invalid" (Left "Model name cannot be empty") (mkModelName "")
        assertEqual "Valid name" (Right "llama3.2") (mkModelName "llama3.2")
    , testCase "GenerateRequest smart constructor" $ do
        let req = generateRequest "llama3.2" "Hello world"
        assertEqual "Model match" (ModelName "llama3.2") (genModel req)
        assertEqual "Prompt match" "Hello world" (genPrompt req)
        assertEqual "Stream default" (Just False) (genStream req)
    , testCase "ChatRequest smart constructor" $ do
        let msg = userMessage "Hi"
            req = chatRequest "llama3.2" (msg :| [])
        assertEqual "Model match" (ModelName "llama3.2") (chatModel req)
        assertEqual "Messages length" 1 (length (chatMessages req))
    , testCase "EmbedRequest smart constructor" $ do
        let req = embedRequest "nomic-embed-text" ["hello", "world"]
        assertEqual "Model match" (ModelName "nomic-embed-text") (embModel req)
        assertEqual "Input match" (Right ["hello", "world"]) (embInput req)
    , testCase "CreateRequest smart constructor" $ do
        let req = defaultCreateRequest "custom-model"
        assertEqual "Model match" (ModelName "custom-model") (crqModel req)
    , testCase "Role JSON roundtrip" $ do
        assertEqual "User role" (Just User) (decode (encode User))
        assertEqual "Assistant role" (Just Assistant) (decode (encode Assistant))
        assertEqual "System role" (Just System) (decode (encode System))
        assertEqual "Tool role" (Just M.Tool) (decode (encode M.Tool))
    , testCase "InMemoryStore operations" $ do
        store <- initInMemoryStore
        now <- getCurrentTime
        let conv =
              Conversation
                { conversationId = "c1"
                , messages = [userMessage "hi"]
                , model = "llama3.2"
                , createdAt = now
                , lastUpdated = now
                }
        saveConversationInMemory store conv
        mLoaded <- loadConversationInMemory store "c1"
        assertEqual "Load conversation" (Just conv) mLoaded

        allConvs <- listConversationsInMemory store
        assertEqual "List conversations" [conv] allConvs

        deleted <- deleteConversationInMemory store "c1"
        assertBool "Delete succeeded" deleted

        mLoadedAfter <- loadConversationInMemory store "c1"
        assertEqual "Load after delete" Nothing mLoadedAfter
    , testCase "Streaming collectStream helper" $ do
        let mockStream = mapM_ yield [1 .. 5 :: Int]
        items <- collectStream mockStream
        assertEqual "Collected list" [1, 2, 3, 4, 5] items
    , testCase "Streaming foldStream helper" $ do
        let mockStream = mapM_ yield [1 .. 5 :: Int]
        sumVal <- foldStream (+) 0 mockStream
        assertEqual "Folded sum" 15 sumVal
    , testCase "GenerateResponse parses image generation progress (no response field)" $ do
        let progressJson =
              "{\"model\":\"x/z-image-turbo\",\"created_at\":\"2026-10-25T10:00:00Z\",\"completed\":5,\"total\":20,\"done\":false}"
        case decode @GenerateResponse progressJson of
          Nothing -> assertFailure "Failed to parse image generation progress JSON"
          Just resp -> do
            assertEqual "Response default" "" (grResponse resp)
            assertEqual "Completed steps" (Just 5) (grCompleted resp)
            assertEqual "Total steps" (Just 20) (grTotal resp)
            assertBool "Not done" (not $ grDone resp)
    , testCase "GenerateResponse parses image generation final response" $ do
        let finalJson =
              "{\"model\":\"x/z-image-turbo\",\"created_at\":\"2026-10-25T10:00:15Z\",\"image\":\"iVBORw0KGgoAAAANSUhEUg==\",\"done\":true,\"done_reason\":\"stop\"}"
        case decode @GenerateResponse finalJson of
          Nothing -> assertFailure "Failed to parse image generation final JSON"
          Just resp -> do
            assertEqual "Response default" "" (grResponse resp)
            assertEqual "Image data" (Just $ Base64Image "iVBORw0KGgoAAAANSUhEUg==") (grImage resp)
            assertBool "Done" (grDone resp)
    , testCase "Message parses tool call response without content field" $ do
        let toolCallJson =
              "{\"role\":\"assistant\",\"tool_calls\":[{\"function\":{\"name\":\"calc\",\"arguments\":{\"x\":1}}}]}"
        case decode @Message toolCallJson of
          Nothing -> assertFailure "Failed to parse tool call message without content"
          Just msg -> do
            assertEqual "Role assistant" Assistant (messageRole msg)
            assertEqual "Content default" "" (messageContent msg)
            assertBool "Tool calls present" (isJust (messageToolCalls msg))
    , testCase "ThinkingLevel supports custom model-defined levels" $ do
        let customLvl = ThinkCustom "xhigh"
            customJson = encode (ThinkLevel customLvl)
        assertEqual "Serialized to string" "\"xhigh\"" customJson
        assertEqual "Roundtrip decode" (Just (ThinkLevel customLvl)) (decode customJson)
    , testCase "Logprob and TokenLogprob roundtrip in ChatResponse and GenerateResponse" $ do
        let logprobData =
              [ Logprob
                  { lpToken = "Hello"
                  , lpLogprob = -0.15
                  , lpTopLogprobs =
                      Just
                        [ TokenLogprob "Hello" (-0.15)
                        , TokenLogprob "Hi" (-1.25)
                        ]
                  }
              ]
            chatJson =
              "{\"model\":\"llama3.2\",\"created_at\":\"2026-10-25T10:00:00Z\",\"done\":true,\"logprobs\":[{\"token\":\"Hello\",\"logprob\":-0.15,\"top_logprobs\":[{\"token\":\"Hello\",\"logprob\":-0.15},{\"token\":\"Hi\",\"logprob\":-1.25}]}]}"
        case decode @ChatResponse chatJson of
          Nothing -> assertFailure "Failed to parse ChatResponse with logprobs"
          Just cResp -> assertEqual "Logprobs match" (Just logprobData) (crLogprobs cResp)
    , testCase "SystemOne questions and answers JSON roundtrip" $ do
        let choiceQ =
              choiceQuestion "Which team?" (Map.fromList [("billing", Just "Billing issues"), ("bugs", Nothing)])
            noulQ = noulQuestion "Is urgent?"
            scoreQ = scoreQuestion "Severity" ["low", "medium", "high"]
            sReq =
              systemOneRequest
                "nimble"
                "I was charged twice"
                (Map.fromList [("team", choiceQ), ("urgent", noulQ), ("severity", scoreQ)])
            encodedReq = encode sReq
        case decode @SystemOneRequest encodedReq of
          Nothing -> assertFailure "Failed to decode SystemOneRequest"
          Just decodedReq -> assertEqual "SystemOneRequest roundtrip" sReq decodedReq

        let ansChoice = AnswerChoice (ChoiceAnswer "billing" (Map.fromList [("billing", 0.95), ("bugs", 0.05)]) 0.95)
            ansNoul = AnswerNoul 0.88
            ansScore =
              AnswerScore
                ( ScoreAnswer
                    2.1
                    (Map.fromList [("0", "low"), ("1", "medium"), ("2", "high")])
                    (Map.fromList [("0", 0.1), ("1", 0.2), ("2", 0.7)])
                    0.7
                )
            resp =
              SystemOneResponse
                { sorModel = "nimble"
                , sorAnswers = Map.fromList [("team", ansChoice), ("urgent", ansNoul), ("severity", ansScore)]
                , sorUsage = SystemOneUsage 42 12
                }
            encodedResp = encode resp
        case decode @SystemOneResponse encodedResp of
          Nothing -> assertFailure "Failed to decode SystemOneResponse"
          Just decodedResp -> assertEqual "SystemOneResponse roundtrip" resp decodedResp
    ]
