module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Ollama

calculatorTool :: Tool
calculatorTool =
  Tool "function" $
    FunctionDef
      { fnName = "add"
      , fnDescription = "Add two numbers"
      , fnParameters =
          FunctionParameters
            "object"
            ( Map.fromList
                [ ("a", object ["type" .= ("number" :: String)])
                , ("b", object ["type" .= ("number" :: String)])
                ]
            )
            ["a", "b"]
      }

main :: IO ()
main = do
  client <- defaultClient
  let req =
        (chatRequest "llama3.2" (userMessage "What is 25 + 17?" :| []))
          { chatTools = Just [calculatorTool]
          }
  res <- chat client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> print (crMessage resp)
