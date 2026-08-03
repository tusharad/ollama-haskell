module Test.Ollama.Golden.Models (tests) where

import Data.Aeson (encode)
import Ollama
import Test.Tasty
import Test.Tasty.Golden

tests :: TestTree
tests =
  testGroup
    "Model Management Golden JSON Serialization Tests"
    [ goldenVsString
        "CreateRequest golden serialization"
        "test/golden/create_request.golden"
        (pure $ encode $ defaultCreateRequest "custom-model")
    ]
