{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Push (
  -- * Push API
  push,pushOps
) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.Int (Int64)
import Network.HTTP.Client

-- TODO: Add Options parameter
data PushOps = PushOps
  { name :: Text,
    insecure :: Maybe Bool,
    stream :: Maybe Bool
  }
  deriving (Show, Eq, Generic, ToJSON)

data PushResp = PushResp
  { status :: Text,
    digest :: Maybe Text,
    total :: Maybe Int64
  }
  deriving (Show, Eq, Generic, FromJSON)

-- | Push a model with options
pushOps :: 
  Text -> -- ^ Model name
  Maybe Bool -> -- ^ Insecure
  Maybe Bool -> -- ^ Stream
  IO ()
pushOps modelName mInsecure mStream = do
  let url = CU.host defaultOllama
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest $ T.unpack (url <> "/api/push")
  let reqBody =
        PushOps
          { name = modelName,
            insecure = mInsecure,
            stream = mStream
          }
      request =
        initialRequest
          { method = "POST",
            requestBody = RequestBodyLBS $ encode reqBody
          }
  withResponse request manager $ \response -> do
    let go = do
          bs <- brRead $ responseBody response
          let eRes = decode (BSL.fromStrict bs) :: Maybe PushResp
          case eRes of
            Nothing -> putStrLn "Something went wrong"
            Just res -> do
              if status res /= "success"
                then do
                  let total' = fromMaybe 0 (total res)
                  putStrLn $ "Remaining bytes: " <> show total'
                  go
                else do
                  putStrLn "Completed"
    go

-- Higher level API for Pull
-- This API is untested. Will test soon!
-- | Push a model
push :: 
  Text -> -- ^ Model name 
  IO ()
push modelName = pushOps modelName Nothing Nothing
