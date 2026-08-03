module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Ollama

calculatorTool :: Tool
calculatorTool =
  Tool
    { toolType = "function"
    , toolFunction =
        FunctionDef
          { fnName = "add"
          , fnDescription = Just "Add two numbers"
          , fnParameters =
              Just
                FunctionParameters
                  { fpType = "object"
                  , fpProperties = Nothing
                  , fpRequired = Just ["a", "b"]
                  , fpAdditionalProperties = Nothing
                  , fpDescription = Nothing
                  , fpEnum = Nothing
                  }
          , fnStrict = Just True
          }
    }

main :: IO ()
main = do
  client <- defaultClient
  let req =
        (chatRequest "qwen3.5:2b" (userMessage "What is 25 + 17?" :| []))
          { chatTools = Just [calculatorTool]
          }
  res <- chat client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> print (crMessage resp)
