{-# LANGUAGE OverloadedStrings #-}

module Example.ChatStructuredOutput (runApp) where

import qualified Data.List.NonEmpty as NE
import Data.Ollama.Chat
import Data.Ollama.Common.SchemaBuilder
import qualified Data.Text.IO as T

{-
# schema = {'type': 'object', 'properties': {'friends': {'type': 'array', 'items': {'type': 'object', 'properties': {'name': {'type': 'string'}, 'age': {'type': 'integer'}, 'is_available': {'type': 'boolean'}}, 'required': ['name', 'age', 'is_available']}}}, 'required': ['friends']}

-}
runApp :: IO ()
runApp = do
  let schema =
        buildSchema $
          emptyObject
            |+ ( "friends"
               , JArray
                  ( JObject
                      ( buildSchema $
                          emptyObject
                            |+ ("name", JString)
                            |+ ("age", JNumber)
                            |+ ("isAvailable", JBoolean)
                            |! "name"
                            |! "age"
                            |! "isAvailable"
                      )
                  )
               )

  let prompt =
        "I have two friends. The first is Ollama 22 years old busy saving the world,"
          <> "and the second is Alonso 23 years old and wants to hang out."
          <> "Return a list of friends in JSON format"
  let messageList = NE.singleton (userMessage prompt)
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
