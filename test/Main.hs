{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Either
import Data.List.NonEmpty hiding (length)
import Data.Maybe
import Data.Ollama.Chat qualified as Chat
import Data.Text qualified as T
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
    , embeddingTest
    , generateFormatTest
    , chatFormatTest
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
                , suffix = Just " [End]"
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
        eRes <-
          Ollama.generate
            defaultGenerateOps
              { modelName = "llama3.2"
              , prompt = "what is 23 + 9?"
              , hostUrl = pure "http://some-site"
              , responseTimeOut = pure 2
              }
        print ("got response" :: String, eRes)
        assertBool "Expecting Left" (isLeft eRes)
    , testCase "Generate with invalid model" $ do
        eRes <- Ollama.generate defaultGenerateOps {modelName = "invalid-model"}
        assertBool "Expecting generation to fail with invalid model" (isLeft eRes)
    ]

generateFormatTest :: TestTree
generateFormatTest =
  testGroup
    "Generate tests with format and options"
    [ testCase "Generate with SchemaFormat and options" $ do
        let schema =
              object
                [ "type" .= ("object" :: String)
                , "properties" .= object ["age" .= object ["type" .= ("integer" :: String)]]
                ]
        eRes <-
          Ollama.generate
            defaultGenerateOps
              { modelName = "llama3.2"
              , prompt = "Ollama is 22 years old and is busy saving the world. Respond using JSON"
              , format = Just (Ollama.SchemaFormat schema)
              }
        case eRes of
          Right res ->
            assertBool
              "Response should contain JSON with key \"age\""
              ("age" `T.isInfixOf` Ollama.response_ res)
          Left err ->
            assertFailure $ "Generation failed with error: " ++ show err
    , testCase "Generate with JsonFormat and options" $ do
        eRes <-
          Ollama.generate
            defaultGenerateOps
              { modelName = "llama3.2"
              , prompt = "Provide a simple JSON response describing Ollama."
              , format = Just Ollama.JsonFormat
              }
        case eRes of
          Right res ->
            assertBool "Response should start with '{'" ("{" `T.isPrefixOf` Ollama.response_ res)
          Left err ->
            assertFailure $ "Generation failed with error: " ++ show err
    ]

chatTest :: TestTree
chatTest =
  testGroup
    "Chat tests"
    [ testCase "chat stream" $ do
        let msg = Ollama.Message User "What is 29 + 3?" Nothing Nothing
            defaultMsg = Ollama.Message User "" Nothing Nothing
        output <-
          capture $
            Ollama.chat
              defaultChatOps
                { Chat.chatModelName = "llama3.2"
                , Chat.messages = msg :| []
                , Chat.stream =
                    Just
                      ( T.putStr
                          . Chat.content
                          . fromMaybe defaultMsg
                          . Chat.message
                      , pure ()
                      )
                }
        assertBool "Checking if chat function is printing anything" (length output > 0)
    , testCase "Chat non-stream" $ do
        let msg = Ollama.Message User "What is 29 + 3?" Nothing Nothing
        eRes <-
          Ollama.chat
            defaultChatOps
              { Chat.chatModelName = "llama3.2"
              , Chat.messages = msg :| []
              }
        assertBool "Checking if chat function returns a valid value" (isRight eRes)
    , testCase "Chat invalid host url" $ do
        let msg = Ollama.Message User "What is 29 + 3?" Nothing Nothing
        eRes <-
          Ollama.chat
            defaultChatOps
              { Chat.chatModelName = "llama3.2"
              , Chat.messages = msg :| []
              , Chat.hostUrl = pure "some random value"
              , Chat.responseTimeOut = pure 2
              }
        assertBool "It should return Left" (isLeft eRes)
    ]

chatFormatTest :: TestTree
chatFormatTest =
  testGroup
    "Chat tests with format and options"
    [ testCase "Chat with SchemaFormat and options" $ do
        let schema =
              object
                [ "type" .= ("object" :: String)
                , "properties" .= object ["age" .= object ["type" .= ("integer" :: String)]]
                ]
            msg =
              Ollama.Message
                User
                "Ollama is 22 years old and is busy saving the world. Respond using JSON"
                Nothing
                Nothing
        eRes <-
          Ollama.chat
            defaultChatOps
              { Chat.chatModelName = "llama3.2"
              , Chat.messages = msg :| []
              , Chat.format = Just (Ollama.SchemaFormat schema)
              , Chat.options = Just $ object ["penalize_newline" .= Bool True]
              }
        case eRes of
          Right res ->
            assertBool "Chat response should contain key \"age\"" $
              maybe False (\m -> "age" `T.isInfixOf` Chat.content m) (Chat.message res)
          Left err ->
            assertFailure $ "Chat failed with error: " ++ show err
    , testCase "Chat with JsonFormat and options" $ do
        let msg = Ollama.Message User "Tell me about Ollama in JSON format." Nothing Nothing
        eRes <-
          Ollama.chat
            defaultChatOps
              { Chat.chatModelName = "llama3.2"
              , Chat.messages = msg :| []
              , Chat.format = Just Ollama.JsonFormat
              }
        case eRes of
          Right res ->
            assertBool "Chat response should start with '{'" $
              maybe False (\m -> "{" `T.isPrefixOf` Chat.content m) (Chat.message res)
          Left err ->
            assertFailure $ "Chat failed with error: " ++ show err
    ]

psTest :: TestTree
psTest =
  testGroup
    "PS test"
    [ testCase "check ps" $ do
        mRes <- Ollama.ps
        assertBool "Check if ps returns anything" (isRight mRes)
    ]

showTest :: TestTree
showTest =
  testGroup
    "Show test"
    [ testCase "check show" $ do
        mRes <- Ollama.showModel "llama3.2"
        assertBool "Check if model exists or not" (isRight mRes)
    ]

embeddingTest :: TestTree
embeddingTest =
  testGroup
    "Embedding test"
    [ testCase "check embedding" $ do
        eRes <- Ollama.embedding "llama3.2" "Why is sky blue?"
        assertBool "Check if embedding returns anything" (isRight eRes)
    ]

main :: IO ()
main = do
  mRes <- Ollama.list
  case mRes of
    Left err -> do
      putStrLn $ "ollama list failed: " <> err
      pure () -- Ollama is likely not running. Not running tests.
    Right _ -> defaultMain tests
