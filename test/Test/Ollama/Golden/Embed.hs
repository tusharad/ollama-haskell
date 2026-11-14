module Test.Ollama.Golden.Embed (tests) where

import Data.Aeson (encode)
import Ollama
import Test.Tasty
import Test.Tasty.Golden

tests :: TestTree
tests =
  testGroup
    "Embed Golden JSON Serialization Tests"
    [ goldenVsString
        "EmbedRequest golden serialization"
        "test/golden/embed_request.golden"
        (pure $ encode $ embedRequest "nomic-embed-text" ["hello", "world"])
    ]
