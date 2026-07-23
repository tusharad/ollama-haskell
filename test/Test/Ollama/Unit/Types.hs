module Test.Ollama.Unit.Types (tests) where

import Ollama.Types.Common
import Ollama.Types.Format
import Ollama.Types.Message
import Ollama.Types.Options
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Unit Types Tests"
    [ testCase "ModelName smart constructor validates non-empty string" $ do
        assertEqual "Empty model name rejected" (Left "Model name cannot be empty") (mkModelName "")
        assertEqual "Valid model name accepted" (Right $ ModelName "llama3.2") (mkModelName "llama3.2")
    , testCase "Duration conversion helper functions" $ do
        let dur = Duration 1500000000 -- 1.5 seconds
        assertEqual "Convert to seconds" 1.5 (durationToSeconds dur)
        assertEqual "Convert to millis" 1500.0 (durationToMillis dur)
    , testCase "Message smart constructors set correct role and defaults" $ do
        let uMsg = userMessage "Hello"
            sMsg = systemMessage "Be helpful"
            aMsg = assistantMessage "Hi there"
            tMsg = toolMessage "Done"
            trMsg = toolResultMessage "42" "calc"
        assertEqual "User message role" User (messageRole uMsg)
        assertEqual "System message role" System (messageRole sMsg)
        assertEqual "Assistant message role" Assistant (messageRole aMsg)
        assertEqual "Tool message role" Tool (messageRole tMsg)
        assertEqual "Tool result message tool_name" (Just "calc") (messageToolName trMsg)
    , testCase "ModelOptions defaultOptions is empty" $ do
        assertEqual "defaultOptions num_keep is Nothing" Nothing (optNumKeep defaultOptions)
        assertEqual "defaultOptions temperature is Nothing" Nothing (optTemperature defaultOptions)
    , testCase "SchemaBuilder constructs structured schemas" $ do
        let builder = emptyObject |+ ("name", JString) |+ ("age", JInteger) |! "name"
            schema = buildSchema builder
        assertEqual "Required fields populated" ["name"] (schemaRequired schema)
    ]
