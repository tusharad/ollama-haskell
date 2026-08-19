{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Ollama.MCP
Description : Model Context Protocol (MCP) tool conversion and server bridging for Ollama using mcp-server.
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Provides direct bidirectional conversion between Ollama function calling definitions ('Tool')
and Model Context Protocol ('ToolDefinition', 'Content', etc.) directly using the official
Hackage @mcp-server@ package.

@since 0.3.0.0
-}
module Ollama.MCP (
  -- * Re-exported MCP.Server Types & Functions
  Content (..),
  ContentImageData (..),
  ContentAudioData (..),
  ResourceContent (..),
  PromptDefinition (..),
  ResourceDefinition (..),
  ToolDefinition (..),
  ArgumentDefinition (..),
  McpSchema,
  pattern McpSchema,
  SchemaType (..),
  schema,
  describedSchema,
  mkToolDefinition,
  mkPromptDefinition,
  mkResourceDefinition,
  McpServerInfo (..),
  McpServerHandlers (..),
  ServerCapabilities (..),
  PromptCapabilities (..),
  ResourceCapabilities (..),
  ToolCapabilities (..),
  LoggingCapabilities (..),
  PromptListHandler,
  PromptGetHandler,
  ResourceListHandler,
  ResourceReadHandler,
  ToolListHandler,
  ToolCallHandler,
  PromptName,
  ToolName,
  ArgumentName,
  ArgumentValue,
  URI,
  parseURI,
  runMcpServerStdio,
  runMcpServerHttp,
  runMcpServerHttpWithConfig,
  HttpConfig (..),
  jsonValueToText,
  McpProtocolError,

  -- * Conversion between Ollama and mcp-server Types
  toolToMcpDefinition,
  mcpDefinitionToTool,
  toolCallToMcpArgs,
  mcpContentToToolOutput,
) where

import Data.Aeson (Value (..), encode)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TEncoding
import MCP.Server (
  HttpConfig (..),
  runMcpServerHttp,
  runMcpServerHttpWithConfig,
  runMcpServerStdio,
 )
import MCP.Server.Types as ServerTypes (
  ArgumentDefinition (..),
  ArgumentName,
  ArgumentValue,
  Content (..),
  ContentAudioData (..),
  ContentImageData (..),
  Error (..),
  LoggingCapabilities (..),
  McpServerHandlers (..),
  McpServerInfo (..),
  PromptCapabilities (..),
  PromptDefinition (..),
  PromptGetHandler,
  PromptListHandler,
  PromptName,
  ResourceCapabilities (..),
  ResourceContent (..),
  ResourceDefinition (..),
  ResourceListHandler,
  ResourceReadHandler,
  Schema (..),
  SchemaType (..),
  ServerCapabilities (..),
  ToolCallHandler,
  ToolCapabilities (..),
  ToolDefinition (..),
  ToolListHandler,
  ToolName,
  URI,
  describedSchema,
  mkPromptDefinition,
  mkResourceDefinition,
  mkToolDefinition,
  parseURI,
  schema,
 )
import Ollama.Types.Tool (
  FunctionDef (..),
  FunctionParameters (..),
  Tool (..),
  ToolCall (..),
  ToolCallFunction (..),
 )

-- | Alias for 'MCP.Server.Types.Error' to prevent collisions with 'Ollama.Error'.
type McpProtocolError = ServerTypes.Error

-- | Alias for 'MCP.Server.Types.Schema' to avoid name collision with 'Ollama.Types.Format.SchemaBuilder.Schema'.
type McpSchema = ServerTypes.Schema

-- | Pattern synonym for matching or constructing an 'McpSchema'.
pattern McpSchema :: Maybe Text -> SchemaType -> McpSchema
pattern McpSchema desc shape = ServerTypes.Schema desc shape

-- | Convert an Aeson 'Value' to 'Text'. Strings are returned unquoted, while other JSON values are serialized.
jsonValueToText :: Value -> Text
jsonValueToText (String t) = t
jsonValueToText v = TL.toStrict (TEncoding.decodeUtf8 (encode v))

-- | Convert an Ollama 'Tool' into an MCP 'ToolDefinition' from @mcp-server@.
toolToMcpDefinition :: Tool -> ToolDefinition
toolToMcpDefinition Tool {toolFunction = FunctionDef {..}} =
  let desc = fromMaybe "" fnDescription
      convertParam :: FunctionParameters -> ServerTypes.Schema
      convertParam p =
        let pDesc = fpDescription p
            shape = case fpType p of
              "string" -> SchemaString (fpEnum p)
              "integer" -> SchemaInteger
              "number" -> SchemaNumber
              "boolean" -> SchemaBoolean
              "array" -> SchemaArray (ServerTypes.Schema Nothing (SchemaString Nothing))
              "object" ->
                let props = maybe [] (\pm -> [(k, convertParam v) | (k, v) <- Map.toList pm]) (fpProperties p)
                    req = fromMaybe [] (fpRequired p)
                 in SchemaObject props req
              _ -> SchemaString Nothing
         in ServerTypes.Schema pDesc shape
      inputSchema = case fnParameters of
        Just p -> convertParam p
        Nothing -> ServerTypes.Schema Nothing (SchemaObject [] [])
   in mkToolDefinition fnName desc inputSchema

-- | Convert an MCP 'ToolDefinition' from @mcp-server@ into an Ollama 'Tool'.
mcpDefinitionToTool :: ToolDefinition -> Tool
mcpDefinitionToTool ToolDefinition {..} =
  let convertSchema :: ServerTypes.Schema -> FunctionParameters
      convertSchema (ServerTypes.Schema mDesc shape) =
        case shape of
          SchemaString mEnum ->
            FunctionParameters
              { fpType = "string"
              , fpProperties = Nothing
              , fpRequired = Nothing
              , fpAdditionalProperties = Nothing
              , fpDescription = mDesc
              , fpEnum = mEnum
              }
          SchemaInteger ->
            FunctionParameters
              { fpType = "integer"
              , fpProperties = Nothing
              , fpRequired = Nothing
              , fpAdditionalProperties = Nothing
              , fpDescription = mDesc
              , fpEnum = Nothing
              }
          SchemaNumber ->
            FunctionParameters
              { fpType = "number"
              , fpProperties = Nothing
              , fpRequired = Nothing
              , fpAdditionalProperties = Nothing
              , fpDescription = mDesc
              , fpEnum = Nothing
              }
          SchemaBoolean ->
            FunctionParameters
              { fpType = "boolean"
              , fpProperties = Nothing
              , fpRequired = Nothing
              , fpAdditionalProperties = Nothing
              , fpDescription = mDesc
              , fpEnum = Nothing
              }
          SchemaArray _itemSchema ->
            FunctionParameters
              { fpType = "array"
              , fpProperties = Nothing
              , fpRequired = Nothing
              , fpAdditionalProperties = Nothing
              , fpDescription = mDesc
              , fpEnum = Nothing
              }
          SchemaObject props req ->
            let propMap = Map.fromList [(k, convertSchema s) | (k, s) <- props]
             in FunctionParameters
                  { fpType = "object"
                  , fpProperties = if Map.null propMap then Nothing else Just propMap
                  , fpRequired = if null req then Nothing else Just req
                  , fpAdditionalProperties = Nothing
                  , fpDescription = mDesc
                  , fpEnum = Nothing
                  }
      params = convertSchema toolDefinitionInputSchema
   in Tool
        { toolType = "function"
        , toolFunction =
            FunctionDef
              { fnName = toolDefinitionName
              , fnDescription = if T.null toolDefinitionDescription then Nothing else Just toolDefinitionDescription
              , fnParameters = Just params
              , fnStrict = Nothing
              }
        }

-- | Convert an Ollama 'ToolCall' into an MCP tool name and arguments list for @mcp-server@.
toolCallToMcpArgs :: ToolCall -> (Text, [(Text, Text)])
toolCallToMcpArgs (ToolCall (ToolCallFunction name args)) =
  (name, [(k, jsonValueToText v) | (k, v) <- Map.toList args])

-- | Extract textual output from an MCP 'Content' (from @mcp-server@).
mcpContentToToolOutput :: Content -> Text
mcpContentToToolOutput (ContentText t) = t
mcpContentToToolOutput (ContentImage (ContentImageData _ mime)) = "[Image: " <> mime <> "]"
mcpContentToToolOutput (ContentAudio (ContentAudioData _ mime)) = "[Audio: " <> mime <> "]"
mcpContentToToolOutput (ContentEmbeddedResource res) =
  "[Resource: " <> T.pack (show (resourceUri res)) <> "]"
mcpContentToToolOutput (ContentResourceLink resDef) =
  "[Resource: " <> resourceDefinitionURI resDef <> "]"
mcpContentToToolOutput (ContentAnnotated _ inner) = mcpContentToToolOutput inner
