module Test.Ollama.Unit.Types (tests) where

import Data.Aeson (decode, encode)
import Data.List.NonEmpty (NonEmpty ((:|)))
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
    ]
