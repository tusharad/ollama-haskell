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
  , chatJson
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

import Data.Aeson
import Data.Aeson.KeyMap qualified as HM
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (isNothing)
import Data.Ollama.Common.Config
import Data.Ollama.Common.Error (OllamaError (..))
import Data.Ollama.Common.Types (ChatResponse (..), Format (..), Message (..), Role (..))
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

-- | Creates a 'Message' with role set to 'System'.
systemMessage :: Text -> Message
systemMessage c = Message {role = System, content = c, images = Nothing, tool_calls = Nothing}

-- | Creates a 'Message' with role set to 'User'.
userMessage :: Text -> Message
userMessage c = Message {role = User, content = c, images = Nothing, tool_calls = Nothing}

-- | Creates a 'Message' with role set to 'Assistant'.
assistantMessage :: Text -> Message
assistantMessage c = Message {role = Assistant, content = c, images = Nothing, tool_calls = Nothing}

-- | Creates a 'Message' with role set to 'Tool'.
toolMessage :: Text -> Message
toolMessage c = Message {role = Tool, content = c, images = Nothing, tool_calls = Nothing}

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
  , tools :: !(Maybe [Value])
  -- ^ Optional tools that may be used in the chat.
  , format :: !(Maybe Format)
  -- ^ An optional format for the chat response (json or JSON schema).
  -- ^ Since 0.1.3.0
  , stream :: !(Maybe (ChatResponse -> IO (), IO ()))
  -- ^ Optional streaming functions where the first handles each chunk of the response, and the second flushes the stream.
  , keepAlive :: !(Maybe Text)
  -- ^ Override default response timeout in minutes. Default = 15 minutes
  , options :: !(Maybe Value)
  -- ^ additional model parameters listed in the documentation for the Modelfile such as temperature
  -- ^ Since 0.1.3.0
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

instance ToJSON ChatOps where
  toJSON (ChatOps model_ messages_ tools_ format_ stream_ keepAlive_ options) =
    object
      [ "model" .= model_
      , "messages" .= messages_
      , "tools" .= tools_
      , "format" .= format_
      , "stream" .= if isNothing stream_ then Just False else Just True
      , "keep_alive" .= keepAlive_
      , "options" .= options
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

To request a JSON format response:

> let ops = defaultChatOps { format = Just JsonFormat }
> result <- chat ops

To request a structured output with a JSON schema:

> import Data.Aeson (object, (.=))
> let ops = defaultChatOps { format = Just (SchemaFormat schema) }
> result <- chat ops
-}
chat :: ChatOps -> Maybe OllamaConfig -> IO (Either OllamaError ChatResponse)
chat ops mbConfig =
  case validateChatOps ops of
    Left err -> return $ Left err
    Right _ -> withOllamaRequest "/api/chat" "POST" (Just ops) mbConfig handler
  where
    handler = case stream ops of
      Nothing -> commonNonStreamingHandler
      Just (sc, fl) -> commonStreamHandler sc fl

{- |
 chatJson is a higher level function that takes ChatOps (similar to chat) and also takes
 a Haskell type (that has To and From JSON instance) and returns the response in provided type.

 This function simply calls chat with extra prompt appended to it, telling LLM to return the
 response in certain JSON format and serializes the response. This function will be helpful when you
 want to use the LLM to do something programmatic.

 Note: This function predates the format parameter in the API. For new code, consider using
 the `format` parameter with a SchemaFormat instead, which leverages the model's native
 JSON output capabilities.

 For Example:
  > let expectedJsonStrucutre = Example {
  >   sortedList = ["sorted List here"]
  > , wasListAlreadSorted = False
  > }
  > let msg0 = Ollama.Message User "Sort given list: [4, 2 , 3, 67].
                        Also tell whether list was already sorted or not." Nothing
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
  Maybe OllamaConfig ->
  -- | Ollama configuration
  jsonResult ->
  -- | Max retries
  Maybe Int ->
  IO (Either OllamaError jsonResult)
chatJson cOps@ChatOps {..} mbConfig jsonStructure mMaxRetries = do
  -- For models that support the format parameter, use that directly
  -- let jsonSchema = encode jsonStructure
  let useNativeFormat = False -- Set to True to use the native format parameter when appropriate
  if useNativeFormat
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
            Nothing -> return $ Left $ ApiError "Something went wrong"
            Just res -> do
              let bs = BSL.fromStrict . T.encodeUtf8 $ content res
              case eitherDecode bs of
                Left err -> return $ Left $ DecodeError err (show bs)
                Right resultInType -> return $ Right resultInType
    else do
      -- Fall back to the original implementation using prompts
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

{- |
   Example usage of 'Ollama.chat' with a JSON schema format and options field.

   The first example sends a message requesting a JSON response conforming to a given schema.
   The second example uses an alternative JSON format (here, @JsonFormat@).

   >>> import Data.Aeson (Value, object, (.=))
   >>> import Data.List.NonEmpty (NonEmpty(..))
   >>> import Ollama (defaultChatOps, Message(..), SchemaFormat, JsonFormat)
   >>> let x :: Value
   ...     x = object [ "type" .= ("object" :: String)
   ...                , "properties" .= object [ "age" .= object ["type" .= ("integer" :: String)] ]
   ...                ]
   >>> let msg = Message User "Ollama is 22 years old and is busy saving the world. Respond using JSON" Nothing
   >>> let opts = object ["option" .= ("some value" :: String)]
   >>> res <- chat defaultChatOps
   ...   { chatModelName = "llama3.2"
   ...   , messages = msg :| []
   ...   , format = Just (SchemaFormat x)
   ...   , options = opts
   ...   }
   >>> print (message res)
   Just (Message {role = Assistant, content = "{\n    \"age\": 22\n}", images = Nothing})
   >>> res2 <- chat defaultChatOps
   ...   { chatModelName = "llama3.2"
   ...   , messages = msg :| []
   ...   , format = Just JsonFormat
   ...   , options = object ["option" .= ("other value" :: String)]
   ...   }
   >>> print (message res2)
   Just (Message {role = Assistant, content = "{ \"Name\": \"Ollama\", \"Age\": 22, \"Occupation\":
   \"World Savior\", \"Goals\": [\"Save humanity from alien invasion\", \"Unite warring nations\", \"Protect the environment\"] }", images = Nothing})
-}
