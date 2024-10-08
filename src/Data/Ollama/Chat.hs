{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Chat (
  -- * Chat APIs
   chat
 , chatOps
 , Message(..)
 , ChatResponse(..)
 -- ** Chat but returning
 , chatOpsReturning
 , chatReturning
 , chatReturning'
 , chatOpsReturning'
 ) where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (UTCTime)
import GHC.Generics
import GHC.Int (Int64)
import Network.HTTP.Client
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Builder (byteString, Builder)

-- TODO: Add Options parameter
data ChatOps = ChatOps
  { model :: Text,
    messages :: [Message],
    tools :: Maybe Text,
    format :: Maybe Text,
    stream :: Maybe Bool,
    keepAlive :: Maybe Text
  }
  deriving (Show, Eq)

-- TODO : Add tool_calls parameter
data Message = Message
  { role :: Text,
    content :: Text,
    images :: Maybe [Text] -- Base64 encoded
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ChatResponse = ChatResponse
  { model :: Text,
    createdAt :: UTCTime,
    message :: Maybe Message,
    done :: Bool,
    totalDuration :: Maybe Int64,
    loadDuration :: Maybe Int64,
    promptEvalCount :: Maybe Int64,
    promptEvalDuration :: Maybe Int64,
    evalCount :: Maybe Int64,
    evalDuration :: Maybe Int64
  }
  deriving (Show, Eq)

instance ToJSON ChatOps where
  toJSON (ChatOps model messages tools format stream keepAlive) =
    object
      [ "model" .= model,
        "messages" .= messages,
        "tools" .= tools,
        "format" .= format,
        "stream" .= stream,
        "keep_alive" .= keepAlive
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

-- | Helper function to construct request and manager for chat
chatOps_ ::
  Text ->
  [Message] ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Text ->
  IO (Request,Manager)
chatOps_ modelName messages mTools mFormat mStream mKeepAlive = do
  let url = CU.host defaultOllama
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest $ T.unpack (url <> "/api/chat")
  let reqBody =
        ChatOps
          { model = modelName,
            messages = messages,
            tools = mTools,
            format = mFormat,
            stream = mStream,
            keepAlive = mKeepAlive
          }
      request =
        initialRequest
          { method = "POST",
            requestBody = RequestBodyLBS $ encode reqBody
          }
  pure (request,manager)

-- | Chat with a given model. It's a lower level API with more options.
chatOps ::
  Text -> -- ^ Model name
  [Message] -> -- ^ Messages
  Maybe Text -> -- ^ Tools
  Maybe Text -> -- ^ Format
  Maybe Bool -> -- ^ Stream
  Maybe Text -> -- ^ Keep Alive
  IO ()
chatOps modelName messages mTools mFormat mStream mKeepAlive = do
  (request,manager) <- chatOps_ modelName messages mTools mFormat mStream mKeepAlive
  withResponse request manager $ \response -> do
    let go = do
          bs <- brRead $ responseBody response
          let eRes = decode (BSL.fromStrict bs) :: Maybe ChatResponse
          case eRes of
            Nothing -> do
              putStrLn "Something went wrong"
            Just res -> do
              unless
                (done res)
                ( do
                    let content' = maybe "" content (message res)
                    T.putStr content'
                    go
                )
    putStrLn "" -- newline after answer ends
    go

chatOpsReturning ::
  Text -> -- ^ Model name
  [Message] -> -- ^ Messages
  Maybe Text -> -- ^ Tools
  Maybe Text -> -- ^ Format
  Maybe Bool -> -- ^ Stream
  Maybe Text -> -- ^ Keep Alive
  (Builder -> IO ()) -> IO () -> IO Text
chatOpsReturning modelName messages mTools mFormat mStream mKeepAlive sendChunk flush = do
  (request,manager) <- chatOps_ modelName messages mTools mFormat mStream mKeepAlive
  withResponse request manager $ \response -> do
      let go output = do
            bs <- brRead $ responseBody response
            if BS.null bs
              then return ""  -- End of stream
              else do
                let eRes = eitherDecode (BSL.fromStrict bs) :: Either String ChatResponse
                case eRes of
                  Left err -> do
                    -- Handle the error case and log it
                    liftIO $ putStrLn $ "Error: " <> err
                    flush
                    return ""
                  Right res -> do
                    -- Send the chunk of response
                    sendChunk $ byteString $ BS.pack $ T.unpack (maybe "" content (message res))
                    flush  -- Ensure data is flushed to the client
                    -- Check if we're done; if not, continue
                    if done res
                      then return output  -- End if processing is done
                    else go (output <> maybe "" content (message res))         -- Continue the loop
      go ""

-- | Non streaming version fo ChatOpsReturning
chatOpsReturning' ::
  Text -> -- ^ Model name
  [Message] -> -- ^ Messages
  Maybe Text -> -- ^ Tools
  Maybe Text -> -- ^ Format
  Maybe Bool -> -- ^ Stream
  Maybe Text -> -- ^ Keep Alive
  IO Text
chatOpsReturning' modelName messages mTools mFormat mStream mKeepAlive = do
  (request,manager) <- chatOps_ modelName messages mTools mFormat mStream mKeepAlive
  withResponse request manager $ \response -> do
      let go output = do
            bs <- brRead $ responseBody response
            if BS.null bs
              then return ""  -- End of stream
              else do
                let eRes = eitherDecode (BSL.fromStrict bs) :: Either String ChatResponse
                case eRes of
                  Left err -> do
                    -- Handle the error case and log it
                    liftIO $ putStrLn $ "Error: " <> err
                    return ""
                  Right res -> do
                    -- Check if we're done; if not, continue
                    if done res
                      then return output  -- End if processing is done
                    else go (output <> maybe "" content (message res))         -- Continue the loop
      go ""

-- | Chat with a given model
chat ::
  Text  -- ^ Model name 
  -> [Message] -- ^ Messages
  -> IO ()
chat modelName messages =
  chatOps modelName messages Nothing Nothing Nothing Nothing

-- | Chat with a given model but returning the response
chatReturning ::
  Text    -- ^ Model name 
  -> [Message] -- ^ Messages
  -> (Builder -> IO ()) -> IO () -> IO Text
chatReturning modelName messages sendChunk flush = do
  chatOpsReturning modelName messages Nothing Nothing (Just True) Nothing sendChunk flush

-- | Non streaming version of chatReturning
chatReturning' ::
  Text    -- ^ Model name 
  -> [Message] -- ^ Messages
  -> IO Text
chatReturning' modelName messages = do
  chatOpsReturning' modelName messages Nothing Nothing (Just True) Nothing