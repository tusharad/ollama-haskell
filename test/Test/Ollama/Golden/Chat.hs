module Test.Ollama.Golden.Chat (tests) where

import Data.Aeson (encode)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Ollama
import Test.Tasty
import Test.Tasty.Golden

tests :: TestTree
tests =
  testGroup
    "Golden JSON Serialization Tests"
    [ goldenVsString
        "ChatRequest JSON wire format matches golden fixture"
        "test/golden/chat_request.golden"
        (pure $ encode $ chatRequest "llama3.2" (userMessage "Why is the sky blue?" :| []))
    , goldenVsString
        "GenerateRequest JSON wire format matches golden fixture"
        "test/golden/generate_request.golden"
        (pure $ encode $ generateRequest "llama3.2" "Why is the sky blue?")
    , goldenVsString
        "EmbedRequest JSON wire format matches golden fixture"
        "test/golden/embed_request.golden"
        (pure $ encode $ embedRequest "nomic-embed-text" ["hello", "world"])
    ]
