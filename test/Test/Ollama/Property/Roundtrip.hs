module Test.Ollama.Property.Roundtrip (tests) where

import Data.Aeson (decode, encode)
import Ollama.Types.Common
import Ollama.Types.Format
import Ollama.Types.Message
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Property & Roundtrip Tests"
    [ testCase "Role JSON roundtrip" $ do
        assertEqual "User role roundtrip" (Just User) (decode $ encode User)
        assertEqual "System role roundtrip" (Just System) (decode $ encode System)
        assertEqual "Assistant role roundtrip" (Just Assistant) (decode $ encode Assistant)
        assertEqual "Tool role roundtrip" (Just Tool) (decode $ encode Tool)
    , testCase "Message JSON roundtrip" $ do
        let msg = userMessage "Test prompt"
        assertEqual "Message roundtrip" (Just msg) (decode $ encode msg)
    , testCase "Format JSON roundtrip" $ do
        assertEqual "JsonFormat roundtrip" (Just JsonFormat) (decode $ encode JsonFormat)
    , testCase "Think and ThinkingLevel JSON roundtrip" $ do
        assertEqual "ThinkEnabled roundtrip" (Just ThinkEnabled) (decode $ encode ThinkEnabled)
        assertEqual "ThinkDisabled roundtrip" (Just ThinkDisabled) (decode $ encode ThinkDisabled)
        assertEqual
          "ThinkLevel High roundtrip"
          (Just $ ThinkLevel ThinkHigh)
          (decode $ encode (ThinkLevel ThinkHigh))
    ]
