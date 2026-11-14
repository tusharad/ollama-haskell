module Main (main) where

import Data.Aeson (Value (Number, String))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ollama

-- | Calculator tool adding two numbers
calculatorTool :: RegisteredTool IO
calculatorTool =
  RegisteredTool
    { rtTool =
        toolDef
          "calculator"
          "Add two numbers together"
          ( objectParams
              [("a", numberParam (Just "First number")), ("b", numberParam (Just "Second number"))]
              ["a", "b"]
          )
    , rtHandler = \args -> case (Map.lookup "a" args, Map.lookup "b" args) of
        (Just (Number a), Just (Number b)) -> pure $ Right (Number (a + b))
        _ -> pure $ Left "Invalid numeric arguments"
    }

-- | Weather report tool
weatherTool :: RegisteredTool IO
weatherTool =
  RegisteredTool
    { rtTool =
        toolDef
          "get_weather"
          "Get current temperature and conditions for a city"
          (objectParams [("city", stringParam (Just "Name of the city"))] ["city"])
    , rtHandler = \args -> case Map.lookup "city" args of
        Just (String city) -> pure $ Right (String (city <> ": 22°C, Sunny with light breeze"))
        _ -> pure $ Left "City parameter missing"
    }

main :: IO ()
main = do
  client <- defaultClient
  putStrLn "=== Autonomous Agent Tool Loop Demo ==="
  let tools = [calculatorTool, weatherTool]
      prompt = userMessage "What is the weather in Tokyo and what is 137 + 482?"
      req = chatRequest "llama3.2:latest" (prompt :| [])
      cfg =
        defaultToolLoopConfig
          { tlcOnTurn = Just $ \turn msg -> do
              putStrLn $ "--- Turn " <> show turn <> " ---"
              case messageToolCalls msg of
                Just tcs -> putStrLn $ "Model requested " <> show (length tcs) <> " tool calls"
                Nothing -> putStrLn "Model reached final response"
          }

  putStrLn "Executing autonomous tool loop..."
  res <- chatWithTools client req tools cfg
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right (finalMsg, history) -> do
      putStrLn "\n=== Final Answer ==="
      putStrLn $ T.unpack (messageContent finalMsg)
      putStrLn $ "\nTotal conversation turns in history: " <> show (length history)
