{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
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
module Ollama.MCP
  ( -- * Re-exported MCP.Server Types & Functions
    Content (..)
  , ContentImageData (..)
  , ContentResourceData (..)
  , ResourceContent (..)
  , PromptDefinition (..)
  , ResourceDefinition (..)
  , ToolDefinition (..)
  , ArgumentDefinition (..)
  , InputSchemaDefinition (..)
  , InputSchemaDefinitionProperty (..)
  , McpServerInfo (..)
  , McpServerHandlers (..)
  , ServerCapabilities (..)
  , PromptCapabilities (..)
  , ResourceCapabilities (..)
  , ToolCapabilities (..)
  , LoggingCapabilities (..)
  , PromptListHandler
  , PromptGetHandler
  , ResourceListHandler
  , ResourceReadHandler
  , ToolListHandler
  , ToolCallHandler
  , PromptName
  , ToolName
  , ArgumentName
  , ArgumentValue
  , URI
  , parseURI
  , runMcpServerStdio
  , runMcpServerHttp
  , runMcpServerHttpWithConfig
  , HttpConfig (..)
  , jsonValueToText
  , McpProtocolError

    -- * Conversion between Ollama and mcp-server Types
  , toolToMcpDefinition
  , mcpDefinitionToTool
  , toolCallToMcpArgs
  , mcpContentToToolOutput
  ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import MCP.Server
  ( HttpConfig (..)
  , jsonValueToText
  , runMcpServerHttp
  , runMcpServerHttpWithConfig
  , runMcpServerStdio
  )
import MCP.Server.Types as ServerTypes
  ( ArgumentDefinition (..)
  , ArgumentName
  , ArgumentValue
  , Content (..)
  , ContentImageData (..)
  , ContentResourceData (..)
  , Error (..)
  , InputSchemaDefinition (..)
  , InputSchemaDefinitionProperty (..)
  , LoggingCapabilities (..)
  , McpServerHandlers (..)
  , McpServerInfo (..)
  , PromptCapabilities (..)
  , PromptDefinition (..)
  , PromptGetHandler
  , PromptListHandler
  , PromptName
  , ResourceCapabilities (..)
  , ResourceContent (..)
  , ResourceDefinition (..)
  , ResourceListHandler
  , ResourceReadHandler
  , ServerCapabilities (..)
  , ToolCapabilities (..)
  , ToolCallHandler
  , ToolDefinition (..)
  , ToolListHandler
  , ToolName
  , URI
  , parseURI
  )
import Ollama.Types.Tool
  ( FunctionDef (..)
  , FunctionParameters (..)
  , Tool (..)
  , ToolCall (..)
  , ToolCallFunction (..)
  )

-- | Alias for 'MCP.Server.Types.Error' to prevent collisions with 'Ollama.Error'.
type McpProtocolError = ServerTypes.Error

-- | Convert an Ollama 'Tool' into an MCP 'ToolDefinition' from @mcp-server@.
toolToMcpDefinition :: Tool -> ToolDefinition
toolToMcpDefinition Tool{ toolFunction = FunctionDef{..} } =
  let desc = maybe "" id fnDescription
      props = case fnParameters of
        Just FunctionParameters{ fpProperties = Just propMap } ->
          [ (k, InputSchemaDefinitionProperty (fpType p) (maybe "" id (fpDescription p)))
          | (k, p) <- Map.toList propMap
          ]
        _ -> []
      req = case fnParameters of
        Just FunctionParameters{ fpRequired = Just r } -> r
        _ -> []
      inputSchema = InputSchemaDefinitionObject props req
  in ToolDefinition
      { toolDefinitionName        = fnName
      , toolDefinitionDescription = desc
      , toolDefinitionInputSchema = inputSchema
      , toolDefinitionTitle       = Nothing
      }

-- | Convert an MCP 'ToolDefinition' from @mcp-server@ into an Ollama 'Tool'.
mcpDefinitionToTool :: ToolDefinition -> Tool
mcpDefinitionToTool ToolDefinition{..} =
  let (propsList, reqList) = case toolDefinitionInputSchema of
        InputSchemaDefinitionObject ps rs -> (ps, rs)
      propMap = Map.fromList
        [ (k, FunctionParameters
            { fpType                 = propertyType
            , fpProperties           = Nothing
            , fpRequired             = Nothing
            , fpAdditionalProperties = Nothing
            , fpDescription          = if T.null propertyDescription then Nothing else Just propertyDescription
            , fpEnum                 = Nothing
            })
        | (k, InputSchemaDefinitionProperty{..}) <- propsList
        ]
      params = FunctionParameters
        { fpType                 = "object"
        , fpProperties           = Just propMap
        , fpRequired             = if null reqList then Nothing else Just reqList
        , fpAdditionalProperties = Nothing
        , fpDescription          = Nothing
        , fpEnum                 = Nothing
        }
  in Tool
      { toolType     = "function"
      , toolFunction = FunctionDef
          { fnName        = toolDefinitionName
          , fnDescription = if T.null toolDefinitionDescription then Nothing else Just toolDefinitionDescription
          , fnParameters  = Just params
          , fnStrict      = Nothing
          }
      }

-- | Convert an Ollama 'ToolCall' into an MCP tool name and arguments list for @mcp-server@.
toolCallToMcpArgs :: ToolCall -> (Text, [(Text, Text)])
toolCallToMcpArgs (ToolCall (ToolCallFunction name args)) =
  (name, [(k, jsonValueToText v) | (k, v) <- Map.toList args])

-- | Extract textual output from an MCP 'Content' (from @mcp-server@).
mcpContentToToolOutput :: Content -> Text
mcpContentToToolOutput (ContentText t)                          = t
mcpContentToToolOutput (ContentImage (ContentImageData _ mime)) = "[Image: " <> mime <> "]"
mcpContentToToolOutput (ContentResource (ContentResourceData uri _)) =
  "[Resource: " <> T.pack (show uri) <> "]"
