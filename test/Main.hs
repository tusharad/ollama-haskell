{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Either
import Data.List.NonEmpty hiding (length)
import Data.Maybe
import Data.Ollama.Chat qualified as Chat
import Data.Text.IO qualified as T
import Ollama (GenerateOps (..), Role (..), defaultChatOps, defaultGenerateOps)
import Ollama qualified
import System.IO.Silently (capture)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ generateTest
    , chatTest
    , psTest
    , showTest
    ]

generateTest :: TestTree
generateTest =
  testGroup
    "Generate tests"
    [ testCase "generate stream" $ do
        output <-
          capture $
            Ollama.generate
              defaultGenerateOps
                { modelName = "llama3.2"
                , prompt = "what is 4 + 2?"
                , stream = Just (T.putStr . Ollama.response_, pure ())
                }
        assertBool "Checking if generate function is printing anything" (length output > 0)
    , testCase "Generate non-stream" $ do
        eRes <-
          Ollama.generate
            defaultGenerateOps
              { modelName = "llama3.2"
              , prompt = "what is 4 + 2?"
              }
        assertBool "Checking if generate function returns a valid value" (isRight eRes)
    , testCase "Generate with invalid host" $ do
        eRes <- Ollama.generate defaultGenerateOps {
            modelName = "llama3.2"
          , prompt = "what is 23 + 9?"
          , hostUrl = pure "http://some-site"
          , responseTimeOut = pure 2
        }
        print ("got response" :: String ,eRes)
        assertBool "Expecting Left" (isLeft eRes)
    , testCase "Generate with invalid model" $ do
        eRes <- Ollama.generate defaultGenerateOps {modelName = "invalid-model"}
        assertBool "Expecting generation to fail with invalid model" (isLeft eRes)
    ]

chatTest :: TestTree
chatTest =
  testGroup
    "Chat tests"
    [ testCase "chat stream" $ do
        let msg = Ollama.Message User "What is 29 + 3?" Nothing
            defaultMsg = Ollama.Message User "" Nothing
        output <-
          capture $
            Ollama.chat
              defaultChatOps
                { Chat.chatModelName = "llama3.2"
                , Chat.messages = msg :| []
                , Chat.stream = Just (T.putStr . Chat.content . fromMaybe defaultMsg . Chat.message, pure ())
                }
        assertBool "Checking if chat function is printing anything" (length output > 0)
    , testCase "Chat non-stream" $ do
        let msg = Ollama.Message User "What is 29 + 3?" Nothing
        eRes <-
          Ollama.chat
            defaultChatOps
              { Chat.chatModelName = "llama3.2"
              , Chat.messages = msg :| []
              }
        assertBool "Checking if chat function returns a valid value" (isRight eRes)
    ]

psTest :: TestTree
psTest =
  testGroup
    "PS test"
    [ testCase "check ps" $ do
        mRes <- Ollama.ps
        assertBool "Check if ps returns anything" (isJust mRes)
    ]

showTest :: TestTree
showTest =
  testGroup
    "Show test"
    [ testCase "check show" $ do
        mRes <- Ollama.showModel "llama3.2"
        assertBool "Check if model exists or not" (isJust mRes)
    ]

main :: IO ()
main = do
  mRes <- Ollama.list
  case mRes of
    Nothing -> pure () -- Ollama is likely not running. Not running tests.
    Just _ -> defaultMain tests
