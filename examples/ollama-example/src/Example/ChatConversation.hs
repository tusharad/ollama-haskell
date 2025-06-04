{-# LANGUAGE OverloadedStrings #-}

module Example.ChatConversation (runApp) where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Ollama.Chat
import qualified Data.Text.IO as T

-- Pass callback function to define what you wanna do with those chunks

repl :: NE.NonEmpty Message -> IO ()
repl msgList = do
  putStr "> "
  userQuestion <- T.getLine
  let msgListWithUser = userMessage userQuestion <| msgList
  eRes <- chat (defaultChatOps {chatModelName = "gemma3", messages = msgListWithUser}) Nothing
  case eRes of
    Left err -> putStrLn $ "Something went wrong: " ++ show err
    Right r -> do
      case message r of
        Nothing -> putStrLn "Something went wrong"
        Just aiMessage -> do
          T.putStrLn $ content aiMessage
          repl (aiMessage <| msgListWithUser)

runApp :: IO ()
runApp = repl (systemMessage "You are a chatbot" :| [])
