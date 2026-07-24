module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
import Ollama
import Test.Tasty
import Test.Tasty.HUnit

testModel :: ModelName
testModel = "qwen3.5:2b"

tests :: TestTree
tests =
  testGroup
    "ollama-haskell Real Server Integration Test Suite"
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
          Right _ -> pure ()
    , testCase "GET /api/ps — listRunning" $ do
        client <- defaultClient
        res <- listRunning client
        case res of
          Left err -> assertFailure $ "List running failed: " <> show err
          Right _ -> pure ()
    , testCase "POST /api/chat — non-streaming chat" $ do
        client <- defaultClient
        let req = chatRequest testModel (userMessage "Say hello in one word." :| [])
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
        let req = chatRequest testModel (userMessage "Count 1 to 3." :| [])
        chunks <- collectStream (chatStream client req)
        assertBool "Stream produced response chunks" (not $ null chunks)
        let lastChunk = last chunks
        assertBool "Final stream chunk is done" (crDone lastChunk)
    , testCase "POST /api/generate — non-streaming generate" $ do
        client <- defaultClient
        let req = generateRequest testModel "Write 3 words."
        res <- generate client req
        case res of
          Left err -> assertFailure $ "Generate request failed: " <> show err
          Right resp -> do
            assertBool "Generate response done" (grDone resp)
            assertBool "Generated response non-empty" (not $ T.null $ grResponse resp)
    , testCase "POST /api/generate — streaming generate with conduit" $ do
        client <- defaultClient
        let req = generateRequest testModel "Say hi."
        chunks <- collectStream (generateStream client req)
        assertBool "Stream produced generate chunks" (not $ null chunks)
        let lastChunk = last chunks
        assertBool "Final generate chunk is done" (grDone lastChunk)
    , testCase "POST /api/embed — vector embeddings" $ do
        client <- defaultClient
        let req = embedRequest testModel ["Hello world", "Haskell LLM client"]
        res <- embed client req
        case res of
          Left (ApiError 501 _) -> pure ()
          Left err -> assertFailure $ "Embed request failed: " <> show err
          Right resp ->
            assertBool "Embeddings non-empty" (not $ null $ erEmbeddings resp)
    , testCase "POST /api/copy & DELETE /api/delete — model copy and delete lifecycle" $ do
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
    ]

main :: IO ()
main = defaultMain tests
