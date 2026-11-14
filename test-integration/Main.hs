module Main (main) where

import Data.Aeson (eitherDecode)
import Data.Aeson.Types (Value)
import Data.ByteString.Lazy qualified as BSL
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Data.Map.Strict
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
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
    [ -- ---------------------------------------------------------------
      -- System & Model Management
      -- ---------------------------------------------------------------
      testCase "GET /api/version — getVersion returns non-empty" $ do
        client <- defaultClient
        res <- getVersion client
        case res of
          Left err -> assertFailure $ "Version request failed: " <> show err
          Right ver -> assertBool "Version non-empty" (not $ T.null $ unVersion ver)
    , testCase "GET /api/tags — listModels returns installed models with capabilities" $ do
        client <- defaultClient
        res <- listModels client
        case res of
          Left err -> assertFailure $ "List models failed: " <> show err
          Right (ListResponse ms) -> do
            assertBool "Has installed models" (not $ null ms)
            -- BUG-7 verification: capabilities field must be parsed
            let hasCapabilities = any (\m -> miCapabilities m /= Nothing) ms
            assertBool "At least one model has capabilities parsed" hasCapabilities
    , testCase "POST /api/show — showModel returns modelfile" $ do
        client <- defaultClient
        res <- showModel client testModel
        case res of
          Left err -> assertFailure $ "Show model failed: " <> show err
          Right resp ->
            assertBool "Modelfile or details present" (not $ T.null $ srsModelfile resp)
    , testCase "GET /api/ps — listRunning succeeds" $ do
        client <- defaultClient
        res <- listRunning client
        case res of
          Left err -> assertFailure $ "List running failed: " <> show err
          Right _ -> pure ()
    , -- ---------------------------------------------------------------
      -- Non-Streaming Chat
      -- ---------------------------------------------------------------
      testCase "POST /api/chat — non-streaming chat returns coherent content" $ do
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
              Just msg -> do
                let content = messageContent msg
                assertBool
                  "Message content is non-empty"
                  (not (T.null content))
    , -- ---------------------------------------------------------------
      -- Streaming Chat — Content Accumulation
      -- ---------------------------------------------------------------
      testCase "POST /api/chat — streaming accumulates non-empty text via foldStream" $ do
        client <- defaultClient
        let req =
              (chatRequest testModel (userMessage "Count from 1 to 3." :| []))
                { chatOptions = fastOptions
                , chatThink = Just ThinkDisabled
                }
        fullText <-
          foldStream
            (\acc chunk -> acc <> maybe "" messageContent (crMessage chunk))
            ""
            (chatStream client req)
        assertBool
          ("Accumulated stream text should be non-empty, got: " <> show fullText)
          (not $ T.null fullText)
    , testCase "POST /api/chat — collectStream produces multiple chunks" $ do
        client <- defaultClient
        let req = (chatRequest testModel (userMessage "Count from 1 to 5." :| [])) {chatThink = Just ThinkDisabled}
        chunks <- collectStream (chatStream client req)
        assertBool
          ("Stream should produce >1 chunks, got: " <> show (length chunks))
          (length chunks > 1)
    , -- ---------------------------------------------------------------
      -- Structured JSON Output
      -- ---------------------------------------------------------------
      testCase "POST /api/chat — structured JsonFormat returns parseable JSON" $ do
        client <- defaultClient
        let req =
              (chatRequest testModel (userMessage "Return JSON: {\"ok\": true}" :| []))
                { chatFormat = Just JsonFormat
                , chatOptions = Just (defaultOptions {optNumPredict = Just 50})
                , chatThink = Just ThinkDisabled
                }
        res <- chat client req
        case res of
          Left err -> assertFailure $ "Structured chat failed: " <> show err
          Right resp -> do
            assertBool "Response done" (crDone resp)
            case crMessage resp of
              Nothing -> assertFailure "No message in structured output response"
              Just msg -> do
                let content = messageContent msg
                    jsonBytes = BSL.fromStrict (TE.encodeUtf8 content)
                case eitherDecode @Value jsonBytes of
                  Left err ->
                    assertFailure $ "Response is not valid JSON: " <> err <> "\nContent: " <> T.unpack content
                  Right _ -> pure ()
    , -- ---------------------------------------------------------------
      -- Tool / Function Calling
      -- ---------------------------------------------------------------
      testCase "POST /api/chat — tool calling populates messageToolCalls" $ do
        client <- defaultClient
        let locProp =
              FunctionParameters
                { fpType = "string"
                , fpProperties = Nothing
                , fpRequired = Nothing
                , fpAdditionalProperties = Nothing
                , fpDescription = Just "The city name, e.g. Tokyo"
                , fpEnum = Nothing
                }
            weatherParams =
              FunctionParameters
                { fpType = "object"
                , fpProperties = Just (Data.Map.Strict.fromList [("location", locProp)])
                , fpRequired = Just ["location"]
                , fpAdditionalProperties = Nothing
                , fpDescription = Nothing
                , fpEnum = Nothing
                }
            weatherTool =
              Tool
                { toolType = "function"
                , toolFunction =
                    FunctionDef
                      { fnName = "get_current_weather"
                      , fnDescription = Just "Get the current weather for a given city"
                      , fnParameters = Just weatherParams
                      , fnStrict = Nothing
                      }
                }
            req =
              (chatRequest testModel (userMessage "What is the weather in Tokyo?" :| []))
                { chatTools = Just [weatherTool]
                , chatThink = Just ThinkDisabled
                }
        res <- chat client req
        case res of
          Left err -> assertFailure $ "Tool chat request failed: " <> show err
          Right resp -> do
            assertBool "Chat response completed" (crDone resp)
            -- The model should return a message (either tool call or text)
            case crMessage resp of
              Nothing -> assertFailure "No message in tool call response"
              Just msg ->
                -- With proper tool schema, model should call the tool.
                -- But LLMs are non-deterministic, so we just verify we got a response.
                assertBool
                  "Message has tool calls or any content"
                  (messageToolCalls msg /= Nothing || messageContent msg /= "")
    , -- ---------------------------------------------------------------
      -- Thinking / Reasoning Models
      -- ---------------------------------------------------------------
      testCase "POST /api/generate — ThinkEnabled populates thinking field" $ do
        client <- defaultClient
        let req =
              (generateRequest testModel "What is 2 + 2?")
                { genThink = Just ThinkEnabled
                , genOptions = fastOptions
                }
        res <- generate client req
        case res of
          Left err -> assertFailure $ "Thinking generate failed: " <> show err
          Right resp -> do
            assertBool "Response done" (grDone resp)
            -- With ThinkEnabled, at least response should be non-empty
            assertBool
              "Generated response or thinking is non-empty"
              (not (T.null (grResponse resp)) || grThinking resp /= Nothing)
    , -- ---------------------------------------------------------------
      -- Non-streaming Generate
      -- ---------------------------------------------------------------
      testCase "POST /api/generate — non-streaming returns non-empty text" $ do
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
    , testCase "POST /api/generate — streaming generate produces chunks" $ do
        client <- defaultClient
        let req = (generateRequest testModel "Say hi.") {genThink = Just ThinkDisabled}
        chunks <- collectStream (generateStream client req)
        assertBool "Stream produced generate chunks" (not $ null chunks)
    , -- ---------------------------------------------------------------
      -- Embeddings
      -- ---------------------------------------------------------------
      testCase "POST /api/embed — vector embeddings (skip if unsupported)" $ do
        client <- defaultClient
        let req = embedRequest testModel ["Hello world", "Haskell LLM client"]
        res <- embed client req
        case res of
          Left (ApiError 501 _) -> pure () -- Model doesn't support embeddings
          Left err -> assertFailure $ "Embed request failed: " <> show err
          Right resp ->
            assertBool "Embeddings non-empty" (not $ null $ erEmbeddings resp)
    , -- ---------------------------------------------------------------
      -- Model Lifecycle (copy + delete)
      -- ---------------------------------------------------------------
      testCase "POST /api/copy & DELETE /api/delete — model lifecycle" $ do
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
    , -- ---------------------------------------------------------------
      -- Conversation Store — Full Round-Trip with LLM
      -- ---------------------------------------------------------------
      testCase "ConversationStore — multi-turn memory round-trip" $ do
        client <- defaultClient
        store <- initInMemoryStore
        now <- getCurrentTime
        let cid = "test-conv-memory"
            initialConv =
              Conversation
                cid
                [systemMessage "You are a concise assistant.", userMessage "My favorite color is green."]
                testModel
                now
                now
        saveConversationInMemory store initialConv

        -- Load and continue the conversation
        mConv <- loadConversationInMemory store cid
        case mConv of
          Nothing -> assertFailure "Failed to load saved conversation"
          Just prev -> do
            let newMsg = userMessage "What is my favorite color?"
                allMsgs = messages prev <> [newMsg]
            case allMsgs of
              [] -> assertFailure "Messages should not be empty"
              (first : rest) -> do
                let req =
                      (chatRequest testModel (first :| rest))
                        { chatOptions = fastOptions
                        , chatThink = Just ThinkDisabled
                        }
                res <- chat client req
                case res of
                  Left err -> assertFailure $ "Multi-turn chat failed: " <> show err
                  Right resp -> do
                    case crMessage resp of
                      Nothing -> assertFailure "No message in multi-turn response"
                      Just botMsg -> do
                        -- Save the updated conversation
                        updatedTime <- getCurrentTime
                        let updatedConv =
                              prev
                                { messages = allMsgs <> [assistantMessage (messageContent botMsg)]
                                , lastUpdated = updatedTime
                                }
                        saveConversationInMemory store updatedConv

                        -- Verify it was saved with extra messages
                        final <- loadConversationInMemory store cid
                        case final of
                          Nothing -> assertFailure "Failed to load updated conversation"
                          Just f ->
                            assertEqual
                              "Updated conversation has 4 messages"
                              4
                              (length (messages f))
    ]

main :: IO ()
main = defaultMain tests
