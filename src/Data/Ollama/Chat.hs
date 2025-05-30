{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module:      Data.Ollama.Chat
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Description: Chat functionality for Ollama client
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental
-}
module Data.Ollama.Chat
  ( -- * Chat APIs
    chat
  , chatM
  , chatJson
  , chatJsonM
  , Message (..)
  , Role (..)
  , defaultChatOps
  , ChatOps (..)
  , ChatResponse (..)
  , Format (..)
  , schemaFromType
  , systemMessage
  , userMessage
  , assistantMessage
  , toolMessage
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Aeson.KeyMap qualified as HM
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (isNothing)
import Data.Ollama.Common.Config
import Data.Ollama.Common.Error (OllamaError (..))
import Data.Ollama.Common.Types
  ( ChatResponse (..)
  , Format (..)
  , InputTool (..)
  , Message (..)
  , ModelOptions
  , Role (..)
  )
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

genMessage :: Role -> Text -> Message
genMessage r c =
  Message
    { role = r
    , content = c
    , images = Nothing
    , tool_calls = Nothing
    , thinking = Nothing
    }

-- | Creates a 'Message' with role set to 'System'.
systemMessage :: Text -> Message
systemMessage c = genMessage System c

-- | Creates a 'Message' with role set to 'User'.
userMessage :: Text -> Message
userMessage c = genMessage User c

-- | Creates a 'Message' with role set to 'Assistant'.
assistantMessage :: Text -> Message
assistantMessage c = genMessage Assistant c

-- | Creates a 'Message' with role set to 'Tool'.
toolMessage :: Text -> Message
toolMessage c = genMessage Tool c

-- | Validates ChatOps to ensure essential fields are not empty
validateChatOps :: ChatOps -> Either OllamaError ChatOps
validateChatOps ops
  | T.null (chatModelName ops) = Left $ InvalidRequest "Chat model name cannot be empty"
  | any (T.null . content) (toList (messages ops)) =
      Left $ InvalidRequest "Messages cannot have empty content"
  | otherwise = Right ops

data ChatOps = ChatOps
  { chatModelName :: !Text
  -- ^ The name of the chat model to be used.
  , messages :: !(NonEmpty Message)
  -- ^ A non-empty list of messages forming the conversation context.
  , tools :: !(Maybe [InputTool])
  -- ^ Optional tools that may be used in the chat.
  , format :: !(Maybe Format)
  -- ^ An optional format for the chat response (json or JSON schema).
  -- ^ Since 0.1.3.0
  , stream :: !(Maybe (ChatResponse -> IO (), IO ()))
  -- ^ Optional streaming functions where the first handles each chunk of the response, and the second flushes the stream.
  , keepAlive :: !(Maybe Int)
  -- ^ Override default response timeout in minutes. Default = 15 minutes
  , options :: !(Maybe ModelOptions)
  -- ^ additional model parameters listed in the documentation for the Modelfile such as temperature
  -- ^ Since 0.1.3.0
  , think :: !(Maybe Bool)
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

{- |
A default configuration for initiating a chat with a model.
This can be used as a starting point and modified as needed.

Example:

> let ops = defaultChatOps { chatModelName = "customModel" }
> chat ops
-}
defaultChatOps :: ChatOps
defaultChatOps =
  ChatOps
    { chatModelName = "llama3.2"
    , messages = userMessage "What is 2+2?" :| []
    , tools = Nothing
    , format = Nothing
    , stream = Nothing
    , keepAlive = Nothing
    , options = Nothing
    , think = Nothing
    }

chat :: ChatOps -> Maybe OllamaConfig -> IO (Either OllamaError ChatResponse)
chat ops mbConfig =
  case validateChatOps ops of
    Left err -> return $ Left err
    Right _ -> withOllamaRequest "/api/chat" "POST" (Just ops) mbConfig handler
  where
    handler = case stream ops of
      Nothing -> commonNonStreamingHandler
      Just (sc, fl) -> commonStreamHandler sc fl

chatM :: MonadIO m => ChatOps -> Maybe OllamaConfig -> m (Either OllamaError ChatResponse)
chatM ops mbCfg = liftIO $ chat ops mbCfg

chatJson ::
  (FromJSON jsonResult, ToJSON jsonResult) =>
  ChatOps ->
  -- | Haskell type that you want your result in
  Maybe OllamaConfig ->
  -- | Ollama configuration
  jsonResult ->
  -- | JSON structure
  Maybe Int ->
  -- | Max retries
  IO (Either OllamaError jsonResult)
chatJson cOps@ChatOps {..} mbConfig jsonStructure mMaxRetries = do
  if isNothing format
    then do
      let schemaList = [("schema", Object $ HM.fromList [("type", String "object")])]
      let formattedOps =
            cOps {format = Just $ SchemaFormat (Object (HM.fromList schemaList))}
      chatResponse <- chat formattedOps mbConfig
      case chatResponse of
        Left err -> return $ Left err
        Right r -> do
          let mMessage = message r
          case mMessage of
            Nothing -> return $ Left $ ApiError "Message not found"
            Just res -> do
              let bs = BSL.fromStrict . T.encodeUtf8 $ content res
              case eitherDecode bs of
                Left err -> return $ Left $ DecodeError err (show bs)
                Right resultInType -> return $ Right resultInType
    else do
      let lastMessage = NonEmpty.last messages
          jsonHelperPrompt =
            "You are an AI that returns only JSON object. \n"
              <> "* Your output should be a JSON object that matches the following schema: \n"
              <> T.decodeUtf8 (BSL.toStrict $ encode jsonStructure)
              <> content lastMessage
              <> "\n"
              <> "# How to treat the task:\n"
              <> "* Stricly follow the schema for the output.\n"
              <> "* Never return anything other than a JSON object.\n"
              <> "* Do not talk to the user.\n"
      chatResponse <-
        chat
          cOps
            { messages =
                NonEmpty.fromList $
                  lastMessage {content = jsonHelperPrompt} : NonEmpty.init messages
            }
          mbConfig
      case chatResponse of
        Left err -> return $ Left err
        Right r -> do
          let mMessage = message r
          case mMessage of
            Nothing -> return $ Left $ ApiError "Something went wrong"
            Just res -> do
              let bs = BSL.fromStrict . T.encodeUtf8 $ content res
              case eitherDecode bs of
                Left err -> do
                  case mMaxRetries of
                    Nothing -> return $ Left $ DecodeError err (show bs)
                    Just n ->
                      if n < 1
                        then return $ Left $ DecodeError err (show bs)
                        else chatJson cOps mbConfig jsonStructure (Just (n - 1))
                Right resultInType -> return $ Right resultInType

-- | Helper function to create a JSON schema from a Haskell type
schemaFromType :: ToJSON a => a -> BSL.ByteString
schemaFromType = encode -- This is a simplified version; a real implementation would generate a JSON Schema

chatJsonM ::
  (MonadIO m, FromJSON jsonResult, ToJSON jsonResult) =>
  ChatOps ->
  -- | Haskell type that you want your result in
  Maybe OllamaConfig ->
  -- | Ollama configuration
  jsonResult ->
  -- | Max retries
  Maybe Int ->
  m (Either OllamaError jsonResult)
chatJsonM ops mbCfg js mbRetry = liftIO $ chatJson ops mbCfg js mbRetry
