{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ollama.Pull
  ( -- * Downloaded Models
    pull
  , pullOps
  ) where

import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Maybe (fromMaybe)
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import GHC.Int (Int64)
import Network.HTTP.Client

-- TODO: Add Options parameter
data PullOps = PullOps
  { name :: Text
  , insecure :: Maybe Bool
  , stream :: Maybe Bool
  }
  deriving (Show, Eq, Generic, ToJSON)

data PullResp = PullResp
  { status :: Text
  , digest :: Maybe Text
  , total :: Maybe Int64
  , completed :: Maybe Int64
  }
  deriving (Show, Eq, Generic, FromJSON)

-- | Pull Model with extra options
pullOps ::
  -- | Model Name
  Text ->
  -- | Insecure
  Maybe Bool ->
  -- | Stream
  Maybe Bool ->
  IO ()
pullOps modelName mInsecure mStream = do
  let url = CU.host defaultOllama
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest $ T.unpack (url <> "/api/pull")
  let reqBody =
        PullOps
          { name = modelName
          , insecure = mInsecure
          , stream = mStream
          }
      request =
        initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode reqBody
          }
  withResponse request manager $ \response -> do
    let go = do
          bs <- brRead $ responseBody response
          let eRes = decode (BSL.fromStrict bs) :: Maybe PullResp
          case eRes of
            Nothing -> putStrLn "Something went wrong"
            Just res -> do
              if status res /= "success"
                then do
                  let completed' = fromMaybe 0 (completed res)
                  let total' = fromMaybe 0 (total res)
                  putStrLn $ "Remaining bytes: " <> show (total' - completed')
                  go
                else do
                  putStrLn "Completed"
    go

-- Higher level API for Pull

-- | Pull Model
pull ::
  -- | Model Name
  Text ->
  IO ()
pull modelName = pullOps modelName Nothing Nothing
