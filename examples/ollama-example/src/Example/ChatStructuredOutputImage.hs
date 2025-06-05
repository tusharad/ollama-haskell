{-# LANGUAGE OverloadedStrings #-}

module Example.ChatStructuredOutputImage (runApp) where

import qualified Data.List.NonEmpty as NE
import Data.Ollama.Chat
import Data.Ollama.Common.SchemaBuilder
import Data.Ollama.Common.Utils (encodeImage)
import qualified Data.Text.IO as T

runApp :: IO ()
runApp = do
  mbEncodedImage <- encodeImage "../sample.png"
  case mbEncodedImage of
    Nothing -> putStrLn "Failed to load image"
    Just encodedImage -> do
      let schema =
            buildSchema $
              emptyObject
                |+ ("summary", JString)
                |+ ("text_color", JString)
                |+ ("background_color", JString)
                |! "summary"
                |! "background_color"

      let prompt =
            "Analyze this image and return a detailed JSON description including objects,"
              <> "colors and any text detected. If you cannot determine certain details,"
              <> " leave those fields empty."
      let userMsg =
            Message
              { role = User
              , content = prompt
              , images = Just [encodedImage]
              , tool_calls = Nothing
              , thinking = Nothing
              }
      let messageList = NE.singleton userMsg
      let ops =
            defaultChatOps
              { chatModelName = "gemma3"
              , messages = messageList
              , format = Just $ SchemaFormat schema
              }
      eRes <- chat ops Nothing
      case eRes of
        Left err -> putStrLn $ "Something went wrong: " ++ show err
        Right r -> do
          putStrLn "LLM response"
          case message r of
            Nothing -> putStrLn "Something went wrong"
            Just (Message _ res _ _ _) -> T.putStrLn res
