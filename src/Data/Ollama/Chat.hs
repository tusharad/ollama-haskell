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
data Message = Message
  { role :: Role
  , content :: Text
  , images :: Maybe [Text] -- Base64 encoded
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- TODO: Add Options parameter
data ChatOps = ChatOps
  { chatModelName :: Text
  , messages :: NonEmpty Message
  , tools :: Maybe Text
  , format :: Maybe Text
  , stream :: Maybe (ChatResponse -> IO (), IO ())
  , keepAlive :: Maybe Text
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
  , createdAt :: UTCTime
  , message :: Maybe Message
  , done :: Bool
  , totalDuration :: Maybe Int64
  , loadDuration :: Maybe Int64
  , promptEvalCount :: Maybe Int64
  , promptEvalDuration :: Maybe Int64
  , evalCount :: Maybe Int64
  , evalDuration :: Maybe Int64
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

-- | Chat with a given model
chat :: ChatOps -> IO (Either String ChatResponse)
chat cOps = do
  let url = CU.host defaultOllama
  manager <- newManager defaultManagerSettings
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
