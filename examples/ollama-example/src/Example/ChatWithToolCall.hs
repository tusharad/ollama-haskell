{-# LANGUAGE OverloadedStrings #-}

module Example.ChatWithToolCall (runApp) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Ollama.Chat
import Data.Scientific

addTwoNumbers :: Int -> Int -> Int
addTwoNumbers = (+)

runApp :: IO ()
runApp = do
  let messageList = NE.singleton (userMessage "What is 23+46? (Use tool)")
      inputTool =
        InputTool
          { toolType = "function"
          , function =
              Function
                { functionName = "addTwoNumbers"
                , description = Just "Add two numbers"
                , parameters =
                    Just
                      FunctionParameters
                        { parameterType = "object"
                        , requiredParams = ["a", "b"]
                        , parameterProperties =
                            object
                              [ "a"
                                  .= object
                                    [ "type" .= String "integer"
                                    , "description" .= String "the first number"
                                    ]
                              , "b"
                                  .= object
                                    [ "type" .= String "integer"
                                    , "description" .= String "the second number"
                                    ]
                              ]
                        , additionalProperties = False
                        }
                , strict = Nothing
                }
          }
      ops =
        defaultChatOps
          { chatModelName = "qwen3:0.6b"
          , messages = messageList
          , tools = Just [inputTool]
          }
  eRes <- chat ops Nothing
  case eRes of
    Left err -> putStrLn $ "Error from chat: " ++ show err
    Right r -> do
      putStrLn "LLM response"
      case message r of
        Nothing -> putStrLn "Message not found from chat response"
        Just msg@(Message _ _ _ mbToolCalls _) -> do
          case mbToolCalls of
            Nothing -> putStrLn $ "No tool calls received from Message" <> show msg
            Just toolCallList -> do
              mapM_ executeFunction toolCallList

executeFunction :: ToolCall -> IO ()
executeFunction (ToolCall func) = do
  if outputFunctionName func == "addTwoNumbers"
    then do
      case arguments func of
        (Object kv) ->
          case KM.lookup "a" kv of
            Nothing -> putStrLn $ "a not found: " <> show kv
            Just (Number firstNum_) -> do
              case KM.lookup "b" kv of
                Nothing -> putStrLn $ "argument b not found: " <> show kv
                Just (Number secondNum_) -> do
                  let firstNum = fromMaybe 0 (toBoundedInteger firstNum_)
                  let secondNum = fromMaybe 0 (toBoundedInteger secondNum_)
                  let res = addTwoNumbers firstNum secondNum
                  print ("result: " :: String, res)
            Just _ -> putStrLn "Only expected object type"
        _ -> putStrLn "Something went wrong"
    else
      putStrLn "Expected addTwoNumbers"
