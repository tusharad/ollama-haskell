module Main (main) where

import Ollama

main :: IO ()
main = do
  client <- defaultClient
  putStrLn "=== Ollama Modern Embeddings API (/api/embed) ==="
  let req = embedRequest "nomic-embed-text" ["Hello world", "Haskell LLM integration"]
  res <- embed client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> do
      let vectors = erEmbeddings resp
      putStrLn $ "Generated " <> show (length vectors) <> " embeddings."
      case vectors of
        (v : _) -> putStrLn $ "Embedding vector dimension: " <> show (length v)
        [] -> putStrLn "No vector generated"

  putStrLn "\n=== Ollama Legacy Embeddings API (/api/embeddings) ==="
  let legacyReq = EmbeddingsRequest (ModelName "nomic-embed-text") "Functional programming in Haskell" Nothing Nothing
  legacyRes <- embeddings client legacyReq
  case legacyRes of
    Left err -> putStrLn $ "Legacy Error: " <> show err
    Right resp ->
      putStrLn $ "Legacy embedding dimension: " <> show (length (ebrEmbedding resp))
