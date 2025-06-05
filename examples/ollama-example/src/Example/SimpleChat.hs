{-# LANGUAGE OverloadedStrings #-}

module Example.SimpleChat (runApp) where

import qualified Data.List.NonEmpty as NE
import Data.Ollama.Chat
import qualified Data.Text.IO as T

runApp :: IO ()
runApp = do
  let messageList = NE.singleton (userMessage "What is the meaning of life?")
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
