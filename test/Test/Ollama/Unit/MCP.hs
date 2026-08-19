{-# LANGUAGE OverloadedStrings #-}

module Test.Ollama.Unit.MCP (tests) where

import Data.Map.Strict qualified as Map
import MCP.Server.Types
  ( Content (..)
  , ContentImageData (..)
  , ContentResourceData (..)
  , InputSchemaDefinition (..)
  , InputSchemaDefinitionProperty (..)
  , ToolDefinition (..)
  , parseURI
  )
import Ollama.MCP
  ( mcpContentToToolOutput
  , mcpDefinitionToTool
  , toolCallToMcpArgs
  , toolToMcpDefinition
  )
import Ollama.Types.Tool
  ( FunctionDef (..)
  , FunctionParameters (..)
  , Tool (..)
  , ToolCall (..)
  , ToolCallFunction (..)
  )
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Unit MCP Integration Tests (mcp-server)"
    [ testCase "toolToMcpDefinition converts Ollama Tool to mcp-server ToolDefinition" $ do
        let propMap = Map.fromList
              [ ("query", FunctionParameters "string" Nothing Nothing Nothing (Just "Search query") Nothing)
              , ("limit", FunctionParameters "integer" Nothing Nothing Nothing (Just "Max results") Nothing)
              ]
            params = FunctionParameters "object" (Just propMap) (Just ["query"]) Nothing Nothing Nothing
            ollamaTool = Tool "function" $ FunctionDef "search" (Just "Search codebase") (Just params) Nothing
            mcpDef = toolToMcpDefinition ollamaTool

        toolDefinitionName mcpDef @?= "search"
        toolDefinitionDescription mcpDef @?= "Search codebase"
        case toolDefinitionInputSchema mcpDef of
          InputSchemaDefinitionObject props req -> do
            req @?= ["query"]
            length props @?= 2
            lookup "query" props @?= Just (InputSchemaDefinitionProperty "string" "Search query")

    , testCase "mcpDefinitionToTool converts mcp-server ToolDefinition to Ollama Tool" $ do
        let props =
              [ ("path", InputSchemaDefinitionProperty "string" "File path")
              , ("content", InputSchemaDefinitionProperty "string" "File content")
              ]
            schema = InputSchemaDefinitionObject props ["path", "content"]
            mcpDef = ToolDefinition "write_file" "Write file to disk" schema Nothing
            ollamaTool = mcpDefinitionToTool mcpDef

        toolType ollamaTool @?= "function"
        let fn = toolFunction ollamaTool
        fnName fn @?= "write_file"
        fnDescription fn @?= Just "Write file to disk"
        case fnParameters fn of
          Just FunctionParameters{..} -> do
            fpType @?= "object"
            fpRequired @?= Just ["path", "content"]
            case fpProperties of
              Just pm -> Map.member "path" pm @?= True
              Nothing -> assertFailure "Expected properties"
          Nothing -> assertFailure "Expected parameters"

    , testCase "toolCallToMcpArgs converts Ollama ToolCall to mcp-server argument pairs" $ do
        let call = ToolCall (ToolCallFunction "greet" (Map.fromList [("name", "Alice")]))
            (fn, args) = toolCallToMcpArgs call
        fn @?= "greet"
        lookup "name" args @?= Just "Alice"

    , testCase "mcpContentToToolOutput extracts text from mcp-server Content" $ do
        let textContent = ContentText "Execution output"
        mcpContentToToolOutput textContent @?= "Execution output"

        let imgContent = ContentImage (ContentImageData "base64..." "image/png")
        mcpContentToToolOutput imgContent @?= "[Image: image/png]"

        case parseURI "file:///workspace/test.txt" of
          Just uri -> do
            let resContent = ContentResource (ContentResourceData uri (Just "text/plain"))
            mcpContentToToolOutput resContent @?= "[Resource: file:///workspace/test.txt]"
          Nothing -> assertFailure "Failed to parse URI"
    ]
