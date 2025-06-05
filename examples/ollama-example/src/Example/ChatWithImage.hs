{-# LANGUAGE OverloadedStrings #-}

module Example.ChatWithImage (runApp) where

import qualified Data.List.NonEmpty as NE
import Data.Ollama.Chat
import qualified Data.Text.IO as T
import Data.Ollama.Common.Utils (encodeImage)

-- Pass callback function to define what you wanna do with those chunks

runApp :: IO ()
runApp = do
  mbEncodedImage <- encodeImage "../sample.png"
  case mbEncodedImage of
    Nothing -> putStrLn "Failed to load image"
    Just encodedImage -> do
      let userMsg = Message {
            role = User
          , content = "What does this image say?"
          , images = Just [encodedImage]
          , tool_calls = Nothing
          , thinking = Nothing
        }
      let messageList = NE.singleton userMsg
      let ops =
            defaultChatOps
              { chatModelName = "gemma3"
              , messages = messageList
              }
      eRes <- chat ops Nothing
      case eRes of
        Left err -> putStrLn $ "Something went wrong: " ++ show err
        Right r -> do
          putStrLn "LLM response"
          case message r of
            Nothing -> putStrLn "Something went wrong"
            Just (Message _ res _ _ _) -> T.putStrLn res
