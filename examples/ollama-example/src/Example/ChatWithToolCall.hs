{-# LANGUAGE OverloadedStrings #-}

module Example.ChatWithToolCall (runApp) where

import Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as HM
import Data.Ollama.Chat
import Data.Scientific

addTwoNumbers :: Int -> Int -> Int
addTwoNumbers = (+)

runApp :: IO ()
runApp = do
  let messageList = NE.singleton (userMessage "What is 23+46? (Use tool)")
      paramProp =
        HM.fromList
          [ ("a", FunctionParameters "number" Nothing Nothing Nothing)
          , ("b", FunctionParameters "number" Nothing Nothing Nothing)
          ]
      functionParams =
        FunctionParameters
          { parameterType = "object"
          , requiredParams = Just ["a", "b"]
          , parameterProperties = Just paramProp
          , additionalProperties = Just False
          }
      functionDef =
        FunctionDef
          { functionName = "addTwoNumbers"
          , functionDescription = Just "Add two numbers"
          , functionParameters = Just functionParams
          , functionStrict = Nothing
          }
      inputTool =
        InputTool
          { toolType = "function"
          , function = functionDef
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

convertToNumber :: Value -> Maybe Int
convertToNumber (Number n) = toBoundedInteger n
convertToNumber _ = Nothing

executeFunction :: ToolCall -> IO ()
executeFunction (ToolCall func) = do
  if outputFunctionName func == "addTwoNumbers"
    then do
      case HM.lookup "a" (arguments func) >>= convertToNumber of
        Nothing -> putStrLn "Parameter a not found"
        Just firstNum_ -> do
          case HM.lookup "b" (arguments func) >>= convertToNumber of
            Nothing -> putStrLn "Parameter b not found"
            Just secondNum_ -> do
              let firstNum = firstNum_
              let secondNum = secondNum_
              let res = addTwoNumbers firstNum secondNum
              print ("result: " :: String, res)
    else
      putStrLn "Expected function name to be addTwoNumbers"
