module Test.Ollama.Golden.Generate (tests) where

import Data.Aeson (encode)
import Ollama
import Test.Tasty
import Test.Tasty.Golden

tests :: TestTree
tests =
  testGroup
    "Generate Golden JSON Serialization Tests"
    [ goldenVsString
        "GenerateRequest golden serialization"
        "test/golden/generate_request.golden"
        (pure $ encode $ generateRequest "llama3.2" "Why is the sky blue?")
    ]
