module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Ollama
import Test.Tasty
import Test.Tasty.HUnit

testModel :: ModelName
testModel = "qwen3.5:2b"

fastOptions :: Maybe ModelOptions
fastOptions = Just (defaultOptions {optNumPredict = Just 15})

tests :: TestTree
tests =
  testGroup
    "ollama-haskell Live Server End-to-End Test Suite"
    [ testCase "GET /api/version — getVersion" $ do
        client <- defaultClient
        res <- getVersion client
        case res of
          Left err -> assertFailure $ "Version request failed: " <> show err
          Right ver -> assertBool "Version non-empty" (not $ T.null $ unVersion ver)
    , testCase "GET /api/tags — listModels" $ do
        client <- defaultClient
        res <- listModels client
        case res of
          Left err -> assertFailure $ "List models failed: " <> show err
          Right (ListResponse ms) ->
            assertBool "Has installed models" (not $ null ms)
    , testCase "POST /api/show — showModel" $ do
        client <- defaultClient
        res <- showModel client testModel
        case res of
          Left err -> assertFailure $ "Show model failed: " <> show err
          Right resp ->
            assertBool "Modelfile or details present" (not $ T.null $ srsModelfile resp)
    , testCase "GET /api/ps — listRunning" $ do
        client <- defaultClient
        res <- listRunning client
        case res of
          Left err -> assertFailure $ "List running failed: " <> show err
          Right _ -> pure ()
    , testCase "POST /api/chat — non-streaming chat" $ do
        client <- defaultClient
        let msgs = systemMessage "You are a helpful assistant." :| [userMessage "Say hello in one word."]
            req = (chatRequest testModel msgs) {chatOptions = fastOptions, chatThink = Just ThinkDisabled}
        res <- chat client req
        case res of
          Left err -> assertFailure $ "Chat request failed: " <> show err
          Right resp -> do
            assertBool "Chat response done" (crDone resp)
            case crMessage resp of
              Nothing -> assertFailure "Expected message in response"
              Just msg ->
                assertBool
                  "Message content or thinking present"
                  (not (T.null (messageContent msg)) || maybe False (not . T.null) (messageThinking msg))
    , testCase "POST /api/chat — streaming chat with conduit" $ do
        client <- defaultClient
        let req = (chatRequest testModel (userMessage "Count from 1 to 5." :| [])) {chatThink = Just ThinkDisabled}
        chunks <- collectStream (chatStream client req)
        assertBool "Stream produced response chunks" (not $ null chunks)
    , testCase "POST /api/generate — non-streaming generate" $ do
        client <- defaultClient
        let req =
              (generateRequest testModel "Write 3 words.")
                { genOptions = fastOptions
                , genThink = Just ThinkDisabled
                }
        res <- generate client req
        case res of
          Left err -> assertFailure $ "Generate request failed: " <> show err
          Right resp -> do
            assertBool "Generate response done" (grDone resp)
            assertBool "Generated response non-empty" (not $ T.null $ grResponse resp)
    , testCase "POST /api/generate — streaming generate with conduit" $ do
        client <- defaultClient
        let req = (generateRequest testModel "Say hi.") {genThink = Just ThinkDisabled}
        chunks <- collectStream (generateStream client req)
        assertBool "Stream produced generate chunks" (not $ null chunks)
    , testCase "POST /api/generate — thinking model support" $ do
        client <- defaultClient
        let req =
              (generateRequest testModel "What is 2 + 2?")
                { genThink = Just ThinkEnabled
                , genOptions = fastOptions
                }
        res <- generate client req
        case res of
          Left err -> assertFailure $ "Thinking generate failed: " <> show err
          Right resp -> assertBool "Response done" (grDone resp)
    , testCase "POST /api/chat — tool calling definition and execution" $ do
        client <- defaultClient
        let weatherTool =
              Tool
                { toolType = "function"
                , toolFunction =
                    FunctionDef
                      { fnName = "get_current_weather"
                      , fnDescription = Just "Get current weather for a city"
                      , fnParameters =
                          Just
                            FunctionParameters
                              { fpType = "object"
                              , fpProperties = Nothing
                              , fpRequired = Just ["location"]
                              , fpAdditionalProperties = Nothing
                              , fpDescription = Nothing
                              , fpEnum = Nothing
                              }
                      , fnStrict = Nothing
                      }
                }
            req =
              (chatRequest testModel (userMessage "What is the weather in Tokyo?" :| []))
                { chatTools = Just [weatherTool]
                , chatOptions = fastOptions
                , chatThink = Just ThinkDisabled
                }
        res <- chat client req
        case res of
          Left err -> assertFailure $ "Tool chat request failed: " <> show err
          Right resp -> assertBool "Chat response completed" (crDone resp)
    , testCase "POST /api/chat — structured output format" $ do
        client <- defaultClient
        let req =
              (chatRequest testModel (userMessage "Respond with JSON listing 2 colors" :| []))
                { chatFormat = Just JsonFormat
                , chatOptions = fastOptions
                , chatThink = Just ThinkDisabled
                }
        res <- chat client req
        case res of
          Left err -> assertFailure $ "Structured chat failed: " <> show err
          Right resp -> assertBool "Response done" (crDone resp)
    , testCase "POST /api/embed — vector embeddings" $ do
        client <- defaultClient
        let req = embedRequest testModel ["Hello world", "Haskell LLM client"]
        res <- embed client req
        case res of
          Left (ApiError 501 _) -> pure ()
          Left err -> assertFailure $ "Embed request failed: " <> show err
          Right resp ->
            assertBool "Embeddings non-empty" (not $ null $ erEmbeddings resp)
    , testCase "POST /api/copy & DELETE /api/delete — model lifecycle" $ do
        client <- defaultClient
        let copyTarget = "qwen3.5:2b-test-copy"
        copyRes <- copyModel client testModel copyTarget
        case copyRes of
          Left err -> assertFailure $ "Copy model failed: " <> show err
          Right () -> do
            delRes <- deleteModel client copyTarget
            case delRes of
              Left err -> assertFailure $ "Delete model failed: " <> show err
              Right () -> pure ()
    , testCase "ConversationStore — InMemoryStore with real conversation" $ do
        store <- initInMemoryStore
        now <- getCurrentTime
        let cid = "test-conv-1"
            conv =
              Conversation
                cid
                [systemMessage "You are a concise assistant.", userMessage "My favorite color is green."]
                testModel
                now
                now
        saveConversationInMemory store conv
        mConv <- loadConversationInMemory store cid
        assertEqual "Loaded saved conversation" (Just conv) mConv
    ]

main :: IO ()
main = defaultMain tests
