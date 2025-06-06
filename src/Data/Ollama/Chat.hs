{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Data.Ollama.Chat
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Chat functionality for interacting with the Ollama API.

This module provides functions and types for initiating and managing chat interactions with an Ollama model.
It includes APIs for sending chat requests, constructing messages with different roles, and configuring chat
operations. The module supports both streaming and non-streaming responses, as well as optional tools and
structured output formats.

The primary functions are 'chat' and 'chatM' for sending chat requests, and helper functions like
'systemMessage', 'userMessage', 'assistantMessage', and 'toolMessage' for constructing messages.
The 'ChatOps' type allows customization of chat parameters, and 'defaultChatOps' provides a convenient
starting point for configuration.

Example:

>>> let ops = defaultChatOps { chatModelName = "customModel", messages = userMessage "Hello!" :| [] }
>>> chat ops Nothing
Either OllamaError ChatResponse
-}
module Data.Ollama.Chat
  ( -- * Chat APIs
    chat
  , chatM

    -- * Message Types
  , Message (..)
  , Role (..)
  , systemMessage
  , userMessage
  , assistantMessage
  , toolMessage
  , genMessage

    -- * Chat Configuration
  , defaultChatOps
  , ChatOps (..)

    -- * Response Types
  , ChatResponse (..)
  , Format (..)

    -- * Configuration and Error Types
  , OllamaConfig (..)
  , defaultOllamaConfig
  , OllamaError (..)
  , ModelOptions (..)
  , defaultModelOptions

    -- * Tool and Function Types
  , InputTool (..)
  , FunctionDef (..)
  , FunctionParameters (..)
  , OutputFunction (..)
  , ToolCall (..)
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (isNothing)
import Data.Ollama.Common.Config
import Data.Ollama.Common.Error (OllamaError (..))
import Data.Ollama.Common.Types
  ( ChatResponse (..)
  , Format (..)
  , FunctionDef (..)
  , FunctionParameters (..)
  , InputTool (..)
  , Message (..)
  , ModelOptions (..)
  , OutputFunction (..)
  , Role (..)
  , ToolCall (..)
  )
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Text qualified as T

{- | Constructs a 'Message' with the specified role and content.

Creates a 'Message' with the given 'Role' and textual content, setting optional fields
('images', 'tool_calls', 'thinking') to 'Nothing'.

Example:

>>> genMessage User "What's the weather like?"
Message {role = User, content = "What's the weather like?", images = Nothing, tool_calls = Nothing, thinking = Nothing}
-}
genMessage :: Role -> Text -> Message
genMessage r c =
  Message
    { role = r
    , content = c
    , images = Nothing
    , tool_calls = Nothing
    , thinking = Nothing
    }

{- | Creates a 'Message' with the 'System' role.

Example:

>>> systemMessage "You are a helpful assistant."
Message {role = System, content = "You are a helpful assistant.", images = Nothing, tool_calls = Nothing, thinking = Nothing}
-}
systemMessage :: Text -> Message
systemMessage c = genMessage System c

{- | Creates a 'Message' with the 'User' role.

Example:

>>> userMessage "What's 2+2?"
Message {role = User, content = "What's 2+2?", images = Nothing, tool_calls = Nothing, thinking = Nothing}
-}
userMessage :: Text -> Message
userMessage c = genMessage User c

{- | Creates a 'Message' with the 'Assistant' role.

Example:

>>> assistantMessage "2+2 equals 4."
Message {role = Assistant, content = "2+2 equals 4.", images = Nothing, tool_calls = Nothing, thinking = Nothing}
-}
assistantMessage :: Text -> Message
assistantMessage c = genMessage Assistant c

{- | Creates a 'Message' with the 'Tool' role.

Example:

>>> toolMessage "Tool output: success"
Message {role = Tool, content = "Tool output: success", images = Nothing, tool_calls = Nothing, thinking = Nothing}
-}
toolMessage :: Text -> Message
toolMessage c = genMessage Tool c

{- | Validates 'ChatOps' to ensure required fields are non-empty.

Checks that the 'chatModelName' is not empty and that no 'Message' in 'messages' has empty content.
Returns 'Right' with the validated 'ChatOps' or 'Left' with an 'OllamaError' if validation fails.
--
-- @since 0.2.0.0
-}
validateChatOps :: ChatOps -> Either OllamaError ChatOps
validateChatOps ops
  | T.null (chatModelName ops) = Left $ InvalidRequest "Chat model name cannot be empty"
  | any (T.null . content) (messages ops) =
      Left $ InvalidRequest "Messages cannot have empty content"
  | otherwise = Right ops

{- | Configuration for initiating a chat with an Ollama model.

Defines the parameters for a chat request, including the model name, messages, and optional settings
for tools, response format, streaming, timeout, and model options.
-}
data ChatOps = ChatOps
  { chatModelName :: !Text
  -- ^ The name of the chat model to be used (e.g., "gemma3").
  , messages :: !(NonEmpty Message)
  -- ^ A non-empty list of messages forming the conversation context.
  , tools :: !(Maybe [InputTool])
  -- ^ Optional tools that may be used in the chat.
  , format :: !(Maybe Format)
  -- ^ Optional format for the chat response (e.g., JSON or JSON schema).
  --
  -- @since 0.1.3.0
  , stream :: !(Maybe (ChatResponse -> IO ()))
  -- ^ Optional callback function to be called with each incoming response.
  , keepAlive :: !(Maybe Int)
  -- ^ Optional override for the response timeout in minutes (default: 15 minutes).
  , options :: !(Maybe ModelOptions)
  -- ^ Optional model parameters (e.g., temperature) as specified in the Modelfile.
  --
  -- @since 0.1.3.0
  , think :: !(Maybe Bool)
  -- ^ Optional flag to enable thinking mode.
  --
  -- @since 0.2.0.0
  }

instance Show ChatOps where
  show
    ( ChatOps
        { chatModelName = m
        , messages = ms
        , tools = t
        , format = f
        , keepAlive = ka
        , think = th
        }
      ) =
      let messagesStr = show (toList ms)
          toolsStr = show t
          formatStr = show f
          keepAliveStr = show ka
          thinkStr = show th
       in T.unpack m
            ++ "\nMessages:\n"
            ++ messagesStr
            ++ "\n"
            ++ toolsStr
            ++ "\n"
            ++ formatStr
            ++ "\n"
            ++ keepAliveStr
            ++ "\n"
            ++ thinkStr

instance Eq ChatOps where
  (==) a b =
    chatModelName a == chatModelName b
      && messages a == messages b
      && tools a == tools b
      && format a == format b
      && keepAlive a == keepAlive b

instance ToJSON ChatOps where
  toJSON (ChatOps model_ messages_ tools_ format_ stream_ keepAlive_ options think_) =
    object
      [ "model" .= model_
      , "messages" .= messages_
      , "tools" .= tools_
      , "format" .= format_
      , "stream" .= if isNothing stream_ then Just False else Just True
      , "keep_alive" .= keepAlive_
      , "options" .= options
      , "think" .= think_
      ]

{- | Default configuration for initiating a chat.

Provides a default 'ChatOps' with the "gemma3" model and a sample user message ("What is 2+2?").
Can be customized by modifying fields as needed.

Example:

>>> let ops = defaultChatOps { chatModelName = "customModel", messages = userMessage "Hello!" :| [] }
>>> chat ops Nothing
Either OllamaError ChatResponse
-}
defaultChatOps :: ChatOps
defaultChatOps =
  ChatOps
    { chatModelName = "gemma3"
    , messages = userMessage "What is 2+2?" :| []
    , tools = Nothing
    , format = Nothing
    , stream = Nothing
    , keepAlive = Nothing
    , options = Nothing
    , think = Nothing
    }

{- | Sends a chat request to the Ollama API.

Validates the 'ChatOps' configuration and sends a POST request to the @\/api\/chat@ endpoint.
Supports both streaming and non-streaming responses based on the 'stream' field in 'ChatOps'.
Returns an 'Either' containing an 'OllamaError' on failure or a 'ChatResponse' on success.

Example:

>>> let ops = defaultChatOps { chatModelName = "gemma3", messages = userMessage "What's the capital of France?" :| [] }
>>> chat ops Nothing
Either OllamaError ChatResponse
-}
chat :: ChatOps -> Maybe OllamaConfig -> IO (Either OllamaError ChatResponse)
chat ops mbConfig =
  case validateChatOps ops of
    Left err -> return $ Left err
    Right _ -> withOllamaRequest "/api/chat" "POST" (Just ops) mbConfig handler
  where
    handler = case stream ops of
      Nothing -> commonNonStreamingHandler
      Just sendChunk -> commonStreamHandler sendChunk

{- | MonadIO version of 'chat' for use in monadic contexts.

Lifts the 'chat' function into a 'MonadIO' context, allowing it to be used in monadic computations.

Example:

>>> import Control.Monad.IO.Class
>>> let ops = defaultChatOps { chatModelName = "gemma3", messages = userMessage "Hello!" :| [] }
>>> runReaderT (chatM ops Nothing) someContext
Either OllamaError ChatResponse
-}
chatM :: MonadIO m => ChatOps -> Maybe OllamaConfig -> m (Either OllamaError ChatResponse)
chatM ops mbCfg = liftIO $ chat ops mbCfg
