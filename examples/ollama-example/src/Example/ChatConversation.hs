{-# LANGUAGE OverloadedStrings #-}

module Example.ChatConversation (runApp) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Ollama.Chat
import qualified Data.Text.IO as T
import System.IO (hFlush, stdout)


repl :: NE.NonEmpty Message -> IO ()
repl msgList = do
  putStr "> "
  hFlush stdout
  userQuestion <- T.getLine
  let msgListWithUser = msgList `NE.appendList` [userMessage userQuestion]
  eRes <- chat (defaultChatOps {chatModelName = "gemma3", messages = msgListWithUser}) Nothing
  case eRes of
    Left err -> putStrLn $ "Something went wrong: " ++ show err
    Right r -> do
      case message r of
        Nothing -> putStrLn "Something went wrong"
        Just aiMessage -> do
          T.putStrLn $ content aiMessage
          repl (msgListWithUser `NE.appendList` [aiMessage])

runApp :: IO ()
runApp = repl (systemMessage "You are a chatbot" :| [])
