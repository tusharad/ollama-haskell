module Main (main) where

import Data.Aeson (Value (String))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ollama

main :: IO ()
main = do
  putStrLn "=== Model Context Protocol (MCP) Bridge Showcase ==="

  putStrLn "\n1. Defining an Ollama Tool..."
  let weatherTool =
        toolDef "get_weather" "Get current weather for a location" $
          objectParams
            [ ("location", stringParam (Just "City and state, e.g. London, UK"))
            , ("units", enumParam ["celsius", "fahrenheit"] (Just "Temperature unit"))
            ]
            ["location"]
  putStrLn $ "Defined tool: " <> T.unpack (fnName (toolFunction weatherTool))

  putStrLn "\n2. Converting Ollama Tool to MCP ToolDefinition (toolToMcpDefinition)..."
  let mcpDef = toolToMcpDefinition weatherTool
  putStrLn $ "MCP Tool Name: " <> T.unpack (toolDefinitionName mcpDef)
  putStrLn $ "MCP Description: " <> T.unpack (toolDefinitionDescription mcpDef)

  putStrLn "\n3. Converting MCP ToolDefinition back to Ollama Tool (mcpDefinitionToTool)..."
  let reconstructed = mcpDefinitionToTool mcpDef
  putStrLn $ "Reconstructed Tool Name: " <> T.unpack (fnName (toolFunction reconstructed))
  putStrLn $ "Reconstructed Tool Type: " <> T.unpack (toolType reconstructed)

  putStrLn "\n4. Converting Ollama ToolCall to MCP argument pairs (toolCallToMcpArgs)..."
  let callArgs =
        Map.fromList
          [ ("location", String "London, UK")
          , ("units", String "celsius")
          ]
      ollamaCall = toolCallWithId "call_weather_99" (mkToolCallFunction "get_weather" callArgs)
      (calledName, mcpArgs) = toolCallToMcpArgs ollamaCall
  putStrLn $ "Target MCP Tool: " <> T.unpack calledName
  putStrLn "Extracted MCP Arguments:"
  mapM_ (\(k, v) -> putStrLn $ "  - " <> T.unpack k <> " = " <> T.unpack v) mcpArgs

  putStrLn "\n5. Extracting text from MCP Content (mcpContentToToolOutput)..."
  let mcpContent = ContentText "Sunny with 21°C in London."
      toolOutput = mcpContentToToolOutput mcpContent
  putStrLn $ "Extracted tool output: " <> T.unpack toolOutput

  putStrLn "\n=== MCP Bridge Showcase Completed Successfully ==="
