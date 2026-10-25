module Main (main) where

import Data.Aeson (Value (Number))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import Ollama

-- Define a tool using the ergonomic toolDef and objectParams smart constructors
calculatorTool :: Tool
calculatorTool =
  toolDef "add" "Add two numbers together" $
    objectParams
      [ ("a", numberParam (Just "First number to add"))
      , ("b", numberParam (Just "Second number to add"))
      ]
      ["a", "b"]

main :: IO ()
main = do
  client <- defaultClient
  putStrLn "=== Step 1: User asks a calculation question ==="
  let userMsg = userMessage "What is 25 + 17?"
      req =
        (chatRequest "llama3.2:latest" (userMsg :| []))
          { chatTools = Just [calculatorTool]
          }

  putStrLn "Sending chat request to Ollama with calculatorTool..."
  res <- chat client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> case crMessage resp of
      Nothing -> putStrLn "No message returned by model."
      Just assistantMsg -> do
        putStrLn "\n=== Step 2: Model responds with tool call ==="
        case messageToolCalls assistantMsg of
          Nothing -> putStrLn "No tool calls generated."
          Just [] -> putStrLn "Tool calls list was empty."
          Just (tc : _) -> do
            let fn = tcFunction tc
                fnName = tcfName fn
                args = tcfArguments fn
                callId = tcId tc
            putStrLn $ "Model requested tool: " <> T.unpack fnName
            putStrLn $ "Call ID: " <> show callId
            putStrLn $ "Arguments: " <> show args

            putStrLn "\n=== Step 3: Executing requested function ==="
            let result = case (Map.lookup "a" args, Map.lookup "b" args) of
                  (Just (Number a), Just (Number b)) ->
                    let sumVal = a + b
                    in T.pack (Sci.formatScientific Sci.Fixed Nothing sumVal)
                  _ -> "Error: invalid arguments"
            putStrLn $ "Calculated result: " <> T.unpack result

            putStrLn "\n=== Step 4: Providing tool result back to model ==="
            let toolMsg = case callId of
                  Just cid -> toolResultMessageWithId result fnName cid
                  Nothing -> toolResultMessage result fnName
                followUpReq =
                  (chatRequest "llama3.2:latest" (userMsg :| [assistantMsg, toolMsg]))
                    { chatTools = Just [calculatorTool]
                    }

            followUpRes <- chat client followUpReq
            case followUpRes of
              Left err -> putStrLn $ "Follow-up error: " <> show err
              Right followUpResp -> case crMessage followUpResp of
                Nothing -> putStrLn "No final response message returned."
                Just finalMsg -> do
                  putStrLn "Final Model Response:"
                  putStrLn (T.unpack (messageFullText finalMsg))
