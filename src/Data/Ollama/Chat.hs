{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Ollama.Chat
  ( -- * Chat APIs
    chat
  , chatJson
  , Message (..)
  , Role (..)
  , defaultChatOps
  , ChatOps (..)
  , ChatResponse (..)
  ) where

import Control.Exception (try)
import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, isNothing)
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
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
  , hostUrl :: Maybe Text
  -- ^ Override default Ollama host url. Default url = "http://127.0.0.1:11434"
  , responseTimeOut :: Maybe Int
  -- ^ Override default response timeout in minutes. Default = 15 minutes
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
  toJSON (ChatOps model_ messages_ tools_ format_ stream_ keepAlive_ _ _) =
    object
      [ "model" .= model_
      , "messages" .= messages_
      , "tools" .= tools_
      , "format" .= format_
      , "stream" .= if isNothing stream_ then Just False else Just True
      , "keep_alive" .= keepAlive_
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
    , hostUrl = Nothing
    , responseTimeOut = Nothing
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
  let url = fromMaybe defaultOllamaUrl (hostUrl cOps)
      responseTimeout = fromMaybe 15 (responseTimeOut cOps)
  manager <-
    newManager
      defaultManagerSettings -- Setting response timeout to 5 minutes, since llm takes time
        { managerResponseTimeout = responseTimeoutMicro (responseTimeout * 60 * 1000000)
        }
  eInitialRequest <- try $ parseRequest $ T.unpack (url <> "/api/chat") :: IO (Either HttpException Request)
  case eInitialRequest of
    Left e -> return $ Left $ "Failed to parse host url: " <> show e
    Right initialRequest -> do
      let reqBody = cOps
          request =
            initialRequest
              { method = "POST"
              , requestBody = RequestBodyLBS $ encode reqBody
              }
      eRes <-
        try (withResponse request manager $ handleRequest cOps) ::
          IO (Either HttpException (Either String ChatResponse))
      case eRes of
        Left e -> return $ Left $ "HTTP error occured: " <> show e
        Right r -> return r

handleRequest :: ChatOps -> Response BodyReader -> IO (Either String ChatResponse)
handleRequest cOps response = do
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

{- |
 chatJson is a higher level function that takes ChatOps (similar to chat) and also takes
 a Haskell type (that has To and From JSON instance) and returns the response in provided type.

 This function simply calls chat with extra prompt appended to it, telling LLM to return the
 response in certain JSON format and serializes the response. This function will be helpful when you
 want to use the LLM to do something programmatic.

 For Example:
  > let expectedJsonStrucutre = Example {
  >   sortedList = ["sorted List here"]
  > , wasListAlreadSorted = False
  > }
  > let msg0 = Ollama.Message User "Sort given list: [4, 2 , 3, 67]. Also tell whether list was already sorted or not." Nothing
  > eRes3 <-
  >  chatJson
  >   defaultChatOps
  >    { Chat.chatModelName = "llama3.2"
  >      , Chat.messages = msg0 :| []
  >   }
  >      expectedJsonStrucutre
  >      (Just 2)
  > print eRes3
 Output:
  > Example {sortedList = ["1","2","3","4"], wasListAlreadSorted = False}

Note: While Passing the type, construct the type that will help LLM understand the field better.
 For example, in the above example, the sortedList's value is written as "Sorted List here". This
 will help LLM understand context better.

 You can also provide number of retries in case the LLM field to return the response in correct JSON
 in first attempt.
-}
chatJson ::
  (FromJSON jsonResult, ToJSON jsonResult) =>
  ChatOps ->
  -- | Haskell type that you want your result in
  jsonResult ->
  -- | Max retries
  Maybe Int ->
  IO (Either String jsonResult)
chatJson cOps@ChatOps {..} jsonStructure mMaxRetries = do
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
  case chatResponse of
    Left err -> return $ Left err
    Right r -> do
      let mMessage = message r
      case mMessage of
        Nothing -> return $ Left "Something went wrong"
        Just res -> do
          case decode (BSL.fromStrict . T.encodeUtf8 $ content res) of
            Nothing -> do
              case mMaxRetries of
                Nothing -> return $ Left "Decoding Failed :("
                Just n -> if n < 1 then return $ Left "Decoding Failed :(" else chatJson cOps jsonStructure (Just (n - 1))
            Just resultInType -> return $ Right resultInType
