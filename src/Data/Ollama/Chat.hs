{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Chat
  ( -- * Chat APIs
    chat
  , Message (..)
  , Role (..)
  , defaultChatOps
  , ChatOps (..)
  , ChatResponse (..)
  ) where

import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.List.NonEmpty
import Data.Maybe (isNothing)
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics
import GHC.Int (Int64)
import Network.HTTP.Client

-- | Enumerated roles that can participate in a chat.
data Role = System | User | Assistant | Tool
  deriving (Show, Eq)

instance ToJSON Role where
  toJSON System = String "system"
  toJSON User = String "user"
  toJSON Assistant = String "assistant"
  toJSON Tool = String "tool"

instance FromJSON Role where
  parseJSON (String "system") = pure System
  parseJSON (String "user") = pure User
  parseJSON (String "assistant") = pure Assistant
  parseJSON (String "tool") = pure Tool
  parseJSON _ = fail "Invalid Role value"

-- TODO : Add tool_calls parameter

-- | Represents a message within a chat, including its role and content.
data Message = Message
  { role :: Role
  -- ^ The role of the entity sending the message (e.g., 'User', 'Assistant').
  , content :: Text
  -- ^ The textual content of the message.
  , images :: Maybe [Text]
  -- ^ Optional list of base64 encoded images that accompany the message.
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- TODO: Add Options parameter
data ChatOps = ChatOps
  { chatModelName :: Text
  -- ^ The name of the chat model to be used.
  , messages :: NonEmpty Message
  -- ^ A non-empty list of messages forming the conversation context.
  , tools :: Maybe Text
  -- ^ Optional tools that may be used in the chat.
  , format :: Maybe Text
  -- ^ An optional format for the chat response.
  , stream :: Maybe (ChatResponse -> IO (), IO ())
  -- ^ Optional streaming functions where the first handles each chunk of the response, and the second flushes the stream.
  , keepAlive :: Maybe Text
  -- ^ Optional text to specify keep-alive behavior.
  }

instance Show ChatOps where
  show (ChatOps {chatModelName = m, messages = ms, tools = t, format = f, keepAlive = ka}) =
    let messagesStr = show (toList ms)
        toolsStr = show t
        formatStr = show f
        keepAliveStr = show ka
     in T.unpack m
          ++ "\nMessages:\n"
          ++ messagesStr
          ++ "\n"
          ++ toolsStr
          ++ "\n"
          ++ formatStr
          ++ "\n"
          ++ keepAliveStr

instance Eq ChatOps where
  (==) a b =
    chatModelName a == chatModelName b
      && messages a == messages b
      && tools a == tools b
      && format a == format b
      && keepAlive a == keepAlive b

data ChatResponse = ChatResponse
  { model :: Text
  -- ^ The name of the model that generated this response.
  , createdAt :: UTCTime
  -- ^ The timestamp when the response was created.
  , message :: Maybe Message
  -- ^ The message content of the response, if any.
  , done :: Bool
  -- ^ Indicates whether the chat process has completed.
  , totalDuration :: Maybe Int64
  -- ^ Optional total duration in milliseconds for the chat process.
  , loadDuration :: Maybe Int64
  -- ^ Optional load duration in milliseconds for loading the model.
  , promptEvalCount :: Maybe Int64
  -- ^ Optional count of prompt evaluations during the chat process.
  , promptEvalDuration :: Maybe Int64
  -- ^ Optional duration in milliseconds for evaluating the prompt.
  , evalCount :: Maybe Int64
  -- ^ Optional count of evaluations during the chat process.
  , evalDuration :: Maybe Int64
  -- ^ Optional duration in milliseconds for evaluations during the chat process.
  }
  deriving (Show, Eq)

instance ToJSON ChatOps where
  toJSON (ChatOps model messages tools format stream keepAlive) =
    object
      [ "model" .= model
      , "messages" .= messages
      , "tools" .= tools
      , "format" .= format
      , "stream" .= if isNothing stream then Just False else Just True
      , "keep_alive" .= keepAlive
      ]

instance FromJSON ChatResponse where
  parseJSON = withObject "ChatResponse" $ \v ->
    ChatResponse
      <$> v .: "model"
      <*> v .: "created_at"
      <*> v .: "message"
      <*> v .: "done"
      <*> v .:? "total_duration"
      <*> v .:? "load_duration"
      <*> v .:? "prompt_eval_count"
      <*> v .:? "prompt_eval_duration"
      <*> v .:? "eval_count"
      <*> v .:? "eval_duration"

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
    , messages = Message User "What is 2+2?" Nothing :| []
    , tools = Nothing
    , format = Nothing
    , stream = Nothing
    , keepAlive = Nothing
    }

{- |
Initiates a chat session with the specified 'ChatOps' configuration and returns either
a 'ChatResponse' or an error message.

This function sends a request to the Ollama chat API with the given options.

Example:

> let ops = defaultChatOps
> result <- chat ops
> case result of
>   Left errorMsg -> putStrLn ("Error: " ++ errorMsg)
>   Right response -> print response
-}
chat :: ChatOps -> IO (Either String ChatResponse)
chat cOps = do
  let url = CU.host defaultOllama
  manager <-
    newManager defaultManagerSettings -- Setting response timeout to 5 minutes, since llm takes time
                    { managerResponseTimeout = responseTimeoutMicro (5 * 60 * 1000000)}
  initialRequest <- parseRequest $ T.unpack (url <> "/api/chat")
  let reqBody = cOps
      request =
        initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode reqBody
          }
  withResponse request manager $ \response -> do
    let streamResponse sendChunk flush = do
          bs <- brRead $ responseBody response
          if BS.null bs
            then putStrLn "" >> pure (Left "")
            else do
              let eRes = eitherDecode (BSL.fromStrict bs) :: Either String ChatResponse
              case eRes of
                Left e -> pure (Left e)
                Right r -> do
                  _ <- sendChunk r
                  _ <- flush
                  if done r then pure (Left "") else streamResponse sendChunk flush
    let genResponse op = do
          bs <- brRead $ responseBody response
          if BS.null bs
            then do
              let eRes = eitherDecode (BSL.fromStrict op) :: Either String ChatResponse
              case eRes of
                Left e -> pure (Left e)
                Right r -> pure (Right r)
            else genResponse (op <> bs)
    case stream cOps of
      Nothing -> genResponse ""
      Just (sendChunk, flush) -> streamResponse sendChunk flush
