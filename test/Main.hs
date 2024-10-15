{-# LANGUAGE OverloadedStrings #-}
module Main where

import Ollama qualified
import Ollama (defaultGenerateOps, GenerateOps(..), defaultChatOps, Role(..))
import Data.Ollama.Chat qualified as Chat
import Data.Text.IO qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Silently (capture)
import Data.Either
import Data.List.NonEmpty hiding (length)
import Data.Maybe

tests :: TestTree
tests = testGroup "Tests" [
       generateTest
      , chatTest
      , psTest
      , showTest
   ]

generateTest :: TestTree
generateTest = testGroup "Generate tests" [
       testCase "generate stream" $ do
          output <- capture $ Ollama.generate defaultGenerateOps { modelName = "smollm:360m"
               , prompt = "what is 4 + 2?"
               , stream = Just (T.putStr . Ollama.response_, pure ()) }
          assertBool "Checking if generate function is printing anything" (length output > 0)
    , testCase "Generate non-stream" $ do
          eRes <- Ollama.generate defaultGenerateOps { modelName = "smollm:360m"
               , prompt = "what is 4 + 2?" }
          assertBool "Checking if generate function returns a valid value" (isRight eRes)
    , testCase "Generate with invalid model" $ do
          eRes <- Ollama.generate defaultGenerateOps { modelName = "invalid-model" }
          assertBool "Expecting generation to fail with invalid model" (isLeft eRes)
   ]

chatTest :: TestTree
chatTest = testGroup "Chat tests" [
        testCase "chat stream" $ do
          let msg = Ollama.Message User "What is 29 + 3?" Nothing
              defaultMsg = Ollama.Message User "" Nothing
          output <- capture $ Ollama.chat defaultChatOps { 
                 Chat.chatModelName = "smollm:360m"
               , Chat.messages = msg :| []
               , Chat.stream = Just (T.putStr . Chat.content . fromMaybe defaultMsg . Chat.message, pure ())
               }
          assertBool "Checking if chat function is printing anything" (length output > 0)

    , testCase "Chat non-stream" $ do
          let msg = Ollama.Message User "What is 29 + 3?" Nothing
          eRes <- Ollama.chat defaultChatOps { 
                 Chat.chatModelName = "smollm:360m"
               , Chat.messages = msg :| []
               }
          assertBool "Checking if chat function returns a valid value" (isRight eRes)
   ]

psTest :: TestTree
psTest = testGroup "PS test" [
       testCase "check ps" $ do
           mRes <- Ollama.ps
           assertBool "Check if ps returns anything" (isJust mRes)
   ]

showTest :: TestTree
showTest = testGroup "Show test" [
       testCase "check show" $ do
           mRes <- Ollama.showModel "smollm:360m"
           assertBool "Check if model exists or not" (isJust mRes)
   ]

main :: IO ()
main = defaultMain tests
