module Main (main) where

import Data.Aeson (FromJSON, eitherDecode)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import GHC.Generics (Generic)
import Ollama

-- | Define your type and derive 'ToSchema'. That's it — no manual schema needed.
data Person = Person
  { name :: Text
  , age :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToSchema)

main :: IO ()
main = do
  client <- defaultClient
  let opts = Just (defaultOptions {optNumPredict = Just 20})
      req =
        (generateRequest "qwen3.5:2b" "Generate a person profile.")
          { genFormat = Just (formatFor @Person)
          , genOptions = opts
          , genThink = Just ThinkDisabled
          }
  res <- generate client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> do
      TIO.putStrLn $ "Raw response:\n" <> grResponse resp
      -- Decode into our typed Person
      case eitherDecode (TLE.encodeUtf8 . TL.fromStrict $ grResponse resp) of
        Left decErr -> putStrLn $ "Decode error: " <> decErr
        Right person -> putStrLn $ "Parsed: " <> show (person :: Person)
