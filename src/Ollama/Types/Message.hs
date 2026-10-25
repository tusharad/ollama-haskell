{- |
Module      : Ollama.Types.Message
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Chat message definitions and helper constructors.

@since 3.0.0.0
-}
module Ollama.Types.Message (
  Role (..),
  Message (..),
  userMessage,
  systemMessage,
  assistantMessage,
  toolMessage,
  toolResultMessage,
  toolResultMessageWithId,
  imageMessage,
  messageFullText,
) where

import Data.Aeson
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics (Generic)
import Ollama.Types.Common (Base64Image)
import Ollama.Types.Tool (ToolCall)

{- | Entity role in a conversation.

@since 3.0.0.0
-}
data Role = System | User | Assistant | Tool
  deriving stock (Eq, Ord, Show, Bounded, Enum, Generic)

instance ToJSON Role where
  toJSON System = String "system"
  toJSON User = String "user"
  toJSON Assistant = String "assistant"
  toJSON Tool = String "tool"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "system" -> pure System
    "user" -> pure User
    "assistant" -> pure Assistant
    "tool" -> pure Tool
    other -> fail $ "Invalid Role: " <> show other

{- | Chat message within a conversation payload.

@since 3.0.0.0
-}
data Message = Message
  { messageRole :: !Role
  , messageContent :: !Text
  , messageImages :: !(Maybe [Base64Image])
  , messageToolCalls :: !(Maybe [ToolCall])
  , messageToolName :: !(Maybe Text)
  , messageThinking :: !(Maybe Text)
  , messageToolCallId :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Message where
  toJSON Message {..} =
    object $
      catMaybes
        [ Just $ "role" .= messageRole
        , Just $ "content" .= messageContent
        , ("images" .=) <$> messageImages
        , ("tool_calls" .=) <$> messageToolCalls
        , ("tool_name" .=) <$> messageToolName
        , ("thinking" .=) <$> messageThinking
        , ("tool_call_id" .=) <$> messageToolCallId
        ]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v ->
    Message
      <$> v .: "role"
      <*> v .:? "content" .!= ""
      <*> v .:? "images"
      <*> v .:? "tool_calls"
      <*> v .:? "tool_name"
      <*> v .:? "thinking"
      <*> v .:? "tool_call_id"

{- | Create a 'User' role message.

@since 3.0.0.0
-}
userMessage :: Text -> Message
userMessage t = Message User t Nothing Nothing Nothing Nothing Nothing

{- | Create a 'System' role message.

@since 3.0.0.0
-}
systemMessage :: Text -> Message
systemMessage t = Message System t Nothing Nothing Nothing Nothing Nothing

{- | Create an 'Assistant' role message.

@since 3.0.0.0
-}
assistantMessage :: Text -> Message
assistantMessage t = Message Assistant t Nothing Nothing Nothing Nothing Nothing

{- | Create a 'Tool' role message.

@since 3.0.0.0
-}
toolMessage :: Text -> Message
toolMessage t = Message Tool t Nothing Nothing Nothing Nothing Nothing

{- | Create a 'Tool' role message with specific @tool_name@ informing the model of tool execution.

@since 3.0.0.0
-}
toolResultMessage :: Text -> Text -> Message
toolResultMessage content toolName =
  Message Tool content Nothing Nothing (Just toolName) Nothing Nothing

{- | Create a 'Tool' role message with specific @tool_name@ and @tool_call_id@.

@since 0.4.2.0
-}
toolResultMessageWithId :: Text -> Text -> Text -> Message
toolResultMessageWithId content toolName callId =
  Message Tool content Nothing Nothing (Just toolName) Nothing (Just callId)

{- | Create a 'User' message with attached Base64 image data.

@since 3.0.0.0
-}
imageMessage :: Text -> [Base64Image] -> Message
imageMessage t imgs = Message User t (Just imgs) Nothing Nothing Nothing Nothing

{- | Retrieve the complete text representation of a message, combining
thinking content and final response content if both are present.

This is especially helpful for reasoning models (e.g., DeepSeek-R1, Qwen 2.5/3.5)
where intermediate tokens are streamed or stored in 'messageThinking'.

@since 0.4.2.0
-}
messageFullText :: Message -> Text
messageFullText msg = case (messageThinking msg, messageContent msg) of
  (Just th, c)
    | not (Text.null th) && not (Text.null c) -> th <> "\n\n" <> c
    | not (Text.null th) -> th
  (_, c) -> c
