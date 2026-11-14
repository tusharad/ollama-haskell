module Main (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Ollama

data PersonInfo = PersonInfo
  { personName :: !Text
  , personAge :: !Int
  , personOccupation :: !Text
  , personHobbies :: ![Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data BookSummary = BookSummary
  { bookTitle :: !Text
  , bookAuthor :: !Text
  , bookGenre :: !Text
  , bookRating :: !Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

main :: IO ()
main = do
  putStrLn "=== Type-Safe Structured Output Example ==="
  client <- defaultClient
  let mName = ModelName "qwen3.5:2b"

  putStrLn "\n1. chatStructured with Generic ToSchema..."
  let msgs =
        userMessage "Give me details for Alan Turing, age 41, mathematician, loves chess and running." :| []
      req = chatRequest mName msgs
  chatRes <- chatStructured @PersonInfo client req
  case chatRes of
    Left err -> putStrLn $ "Error in chatStructured: " <> show err
    Right (StructuredResponse person _raw) -> do
      putStrLn "Successfully decoded PersonInfo:"
      putStrLn $ "  Name:       " <> T.unpack (personName person)
      putStrLn $ "  Age:        " <> show (personAge person)
      putStrLn $ "  Occupation: " <> T.unpack (personOccupation person)
      putStrLn $ "  Hobbies:    " <> show (personHobbies person)

  putStrLn "\n2. generateStructured with Generic ToSchema..."
  let genReq =
        generateRequest mName "Generate details for the book '1984' by George Orwell in dystopian fiction."
  genRes <- generateStructured @BookSummary client genReq
  case genRes of
    Left err -> putStrLn $ "Error in generateStructured: " <> show err
    Right (book, _raw) -> do
      putStrLn "Successfully decoded BookSummary:"
      putStrLn $ "  Title:  " <> T.unpack (bookTitle book)
      putStrLn $ "  Author: " <> T.unpack (bookAuthor book)
      putStrLn $ "  Genre:  " <> T.unpack (bookGenre book)
      putStrLn $ "  Rating: " <> show (bookRating book)
