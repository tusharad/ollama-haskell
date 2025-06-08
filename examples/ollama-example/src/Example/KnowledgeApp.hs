{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Example.KnowledgeApp (runApp, exampleUsage) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad
import System.Directory
import Data.Time
import GHC.Generics
import Ollama
import Data.Ollama.Chat
import Data.Aeson
import qualified Data.ByteString.Lazy as L

data Note = Note
  { noteId :: !Int
  , noteTitle :: !Text
  , noteContent :: !Text
  , noteTags :: ![Text]
  , noteCreated :: !UTCTime
  , noteModified :: !UTCTime
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data KnowledgeBase = KnowledgeBase
  { notes :: ![Note]
  , nextId :: !Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Command
  = AddNote Text Text [Text]
  | SearchNotes Text
  | ListNotes
  | AskQuestion Text
  | SummarizeNotes [Text]
  | Help
  | Quit
  deriving (Show, Eq)

knowledgeBaseFile :: FilePath
knowledgeBaseFile = "knowledge_base.json"

loadKnowledgeBase :: IO KnowledgeBase
loadKnowledgeBase = do
  exists <- doesFileExist knowledgeBaseFile
  if exists
    then do
      content <- L.readFile knowledgeBaseFile
      case decode content of
        Just kb -> return kb
        Nothing -> return emptyKnowledgeBase
    else return emptyKnowledgeBase

saveKnowledgeBase :: KnowledgeBase -> IO ()
saveKnowledgeBase kb = L.writeFile knowledgeBaseFile (encode kb)

emptyKnowledgeBase :: KnowledgeBase
emptyKnowledgeBase = KnowledgeBase [] 1

addNote :: Text -> Text -> [Text] -> KnowledgeBase -> IO KnowledgeBase
addNote title content tags kb = do
  now <- getCurrentTime
  let note = Note
        { noteId = nextId kb
        , noteTitle = title
        , noteContent = content
        , noteTags = tags
        , noteCreated = now
        , noteModified = now
        }
  let newKb = kb { notes = note : notes kb, nextId = nextId kb + 1 }
  saveKnowledgeBase newKb
  putStrLn $ "✓ Added note: " ++ T.unpack title
  return newKb

searchNotes :: Text -> KnowledgeBase -> IO [Note]
searchNotes query kb = do
  let matchingNotes = filter (matchesQuery query) (notes kb)
  putStrLn $ "Found " ++ show (length matchingNotes) ++ " matching notes:"
  mapM_ printNotePreview matchingNotes
  return matchingNotes
  where
    matchesQuery q note =
      T.isInfixOf (T.toLower q) (T.toLower $ noteTitle note) ||
      T.isInfixOf (T.toLower q) (T.toLower $ noteContent note) ||
      any (T.isInfixOf (T.toLower q) . T.toLower) (noteTags note)

listNotes :: KnowledgeBase -> IO ()
listNotes kb = do
  putStrLn $ "Total notes: " ++ show (length $ notes kb)
  mapM_ printNotePreview (take 10 $ notes kb)
  when (length (notes kb) > 10) $
    putStrLn "... (showing first 10 notes)"

printNotePreview :: Note -> IO ()
printNotePreview note = do
  putStrLn $ "  [" ++ show (noteId note) ++ "] " ++ T.unpack (noteTitle note)
  putStrLn $ "      Tags: " ++ T.unpack (T.intercalate ", " (noteTags note))
  putStrLn $ "      " ++ T.unpack (T.take 100 (noteContent note)) ++ "..."
  putStrLn ""

getAnsweredMessage :: ChatResponse -> Maybe Text
getAnsweredMessage ChatResponse{..} = do
  case message of
    Nothing -> Nothing
    Just Message{..} -> Just content

askQuestion :: Text -> KnowledgeBase -> IO ()
askQuestion question kb = do
  putStrLn "🤔 Thinking..."

  let context = createContext kb
  print ("context is ":: String, context)
  let systemPrompt = "You are a helpful personal knowledge assistant. " <>
                    "Use the provided context from the user's notes to answer their question. " <>
                    "If the answer isn't in the context, say so politely. " <>
                    "Context:\n" <> context

  let chatOps = defaultChatOps
        { chatModelName = "gemma3"
        , messages =
            genMessage System systemPrompt :| [genMessage User question]
        }

  result <- chat chatOps Nothing
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right response -> do
      case getAnsweredMessage response of
        Nothing -> putStrLn "Something went wrong"
        Just r -> do
            putStrLn "🤖 Assistant:"
            TIO.putStrLn r

createContext :: KnowledgeBase -> Text
createContext kb =
  let relevantNotes = notes kb
      noteTexts = map formatNoteForContext relevantNotes
  in T.intercalate "\n---\n" noteTexts
  where
    formatNoteForContext note =
      "Title: " <> noteTitle note <> "\n" <>
      "Tags: " <> T.intercalate ", " (noteTags note) <> "\n" <>
      "Content: " <> noteContent note

summarizeNotes :: [Text] -> KnowledgeBase -> IO ()
summarizeNotes tags kb = do
  let filteredNotes = if null tags
                     then notes kb
                     else filter (hasAnyTag tags) (notes kb)

  if null filteredNotes
    then putStrLn "No notes found with the specified tags."
    else do
      putStrLn "📝 Generating summary..."

      let notesText = T.intercalate "\n---\n" $ map formatNoteForSummary filteredNotes
      let prompt = "Please provide a concise summary of these notes, highlighting key themes and insights:\n\n" <> notesText

      let chatOps = defaultChatOps
            { chatModelName = "gemma3"
            , messages = genMessage User prompt :| []
            }

      result <- chat chatOps Nothing
      case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right response -> do
          case getAnsweredMessage response of
            Nothing -> putStrLn "Something went wrong"
            Just r -> do
                putStrLn "📋 Summary:"
                TIO.putStrLn r
  where
    hasAnyTag targetTags note = any (`elem` noteTags note) targetTags
    formatNoteForSummary note =
      noteTitle note <> "\n" <> noteContent note

parseCommand :: Text -> Maybe Command
parseCommand input =
  case T.words input of
    ["add", title] -> Just $ AddNote title "" []
    "add":title:rest -> Just $ AddNote title (T.unwords rest) []
    ["search", query] -> Just $ SearchNotes query
    "search":terms -> Just $ SearchNotes (T.unwords terms)
    ["list"] -> Just ListNotes
    ["ask"] -> Nothing
    "ask":question -> Just $ AskQuestion (T.unwords question)
    ["summarize"] -> Just $ SummarizeNotes []
    "summarize":tags -> Just $ SummarizeNotes tags
    ["help"] -> Just Help
    ["quit"] -> Just Quit
    ["exit"] -> Just Quit
    _ -> Nothing

processCommand :: Command -> KnowledgeBase -> IO KnowledgeBase
processCommand cmd kb = case cmd of
  AddNote title content tags -> do
    if T.null content
      then do
        putStrLn "Enter note content (end with empty line):"
        noteContent <- readMultilineInput
        addNote title noteContent tags kb
      else addNote title content tags kb

  SearchNotes query -> do
    _ <- searchNotes query kb
    return kb

  ListNotes -> do
    listNotes kb
    return kb

  AskQuestion question -> do
    askQuestion question kb
    return kb

  SummarizeNotes tags -> do
    summarizeNotes tags kb
    return kb

  Help -> do
    showHelp
    return kb

  Quit -> return kb

readMultilineInput :: IO Text
readMultilineInput = do
  lines' <- readLines []
  return $ T.intercalate "\n" (reverse lines')
  where
    readLines acc = do
      line <- TIO.getLine
      if T.null line
        then return acc
        else readLines (line : acc)

showHelp :: IO ()
showHelp = putStrLn $ unlines
  [ "Personal Knowledge Assistant Commands:"
  , ""
  , "  add <title> [content]    - Add a new note"
  , "  search <query>           - Search notes by content"
  , "  list                     - List all notes"
  , "  ask <question>           - Ask AI about your notes"
  , "  summarize [tags...]      - Generate AI summary of notes"
  , "  help                     - Show this help"
  , "  quit/exit                - Exit the application"
  , ""
  , "Examples:"
  , "  add \"Meeting Notes\" Today we discussed the project timeline"
  , "  search project"
  , "  ask What did we decide about the timeline?"
  , "  summarize meeting work"
  ]

-- Main application loop
mainLoop :: KnowledgeBase -> IO ()
mainLoop kb = do
  putStr "Knowledge> "
  input <- TIO.getLine

  case parseCommand input of
    Nothing -> do
      putStrLn "Invalid command. Type 'help' for available commands."
      mainLoop kb

    Just Quit -> putStrLn "Goodbye!"

    Just cmd -> do
      newKb <- processCommand cmd kb
      mainLoop newKb

runApp :: IO ()
runApp = do
  putStrLn "Personal Knowledge Assistant with Ollama"
  putStrLn "Loading knowledge base..."
  kb <- loadKnowledgeBase
  putStrLn $ "Loaded " ++ show (length $ notes kb) ++ " notes."
  putStrLn "Type 'help' for available commands.\n"
  mainLoop kb

exampleUsage :: IO ()
exampleUsage = do
  let kb = emptyKnowledgeBase
  kb1 <- addNote "Project Planning"
                 "We need to complete the MVP by Q2. Key features include user authentication, data visualization, and reporting."
                 ["work", "project", "planning"]
                 kb

  kb2 <- addNote "Reading List"
                 "Books to read: Clean Code, Design Patterns, Haskell Programming from First Principles"
                 ["books", "learning", "programming"]
                 kb1

  kb3 <- addNote "Recipe Ideas"
                 "Try making: Thai green curry, homemade pasta, chocolate chip cookies"
                 ["cooking", "recipes", "food"]
                 kb2

  putStrLn "\n=== Demo: Searching for 'project' ==="
  _ <- searchNotes "project" kb3

  putStrLn "\n=== Demo: Asking AI about project timeline ==="
  askQuestion "What's the timeline for the MVP?" kb3

  putStrLn "\n=== Demo: Summarizing work-related notes ==="
  summarizeNotes ["work", "project"] kb3
