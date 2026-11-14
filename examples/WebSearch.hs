module Main (main) where

import Data.Text qualified as T
import Ollama

main :: IO ()
main = do
  putStrLn "=== Ollama Cloud Web Grounding Example ==="
  client <- defaultClient

  putStrLn "\n1. Web Search (webSearch)..."
  let searchReq = webSearchRequest "Haskell programming language latest release"
  searchRes <- webSearch client searchReq
  case searchRes of
    Left err -> putStrLn $ "Search result (or auth required): " <> show err
    Right (WebSearchResponse results) -> do
      putStrLn $ "Found " <> show (length results) <> " search results:"
      mapM_
        ( \r ->
            putStrLn $
              "  - ["
                <> T.unpack (srTitle r)
                <> "] ("
                <> T.unpack (srUrl r)
                <> ")\n    "
                <> T.unpack (T.take 120 (srContent r))
                <> "..."
        )
        results

  putStrLn "\n2. Web Fetch (webFetch)..."
  let fetchReq = webFetchRequest "https://www.haskell.org"
  fetchRes <- webFetch client fetchReq
  case fetchRes of
    Left err -> putStrLn $ "Fetch result (or auth required): " <> show err
    Right resp -> do
      putStrLn $ "Fetched Title: " <> T.unpack (wfrTitle resp)
      putStrLn $ "Content snippet: " <> T.unpack (T.take 200 (wfrContent resp)) <> "..."
