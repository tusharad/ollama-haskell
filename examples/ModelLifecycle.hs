module Main (main) where

import Ollama

main :: IO ()
main = do
  client <- defaultClient
  let sourceModel = ModelName "llama3.2:latest"
      destModel = ModelName "llama-backup-test:latest"
      customModel = ModelName "haskell-expert:latest"

  putStrLn "=== 1. Copying Model (copyModel) ==="
  putStrLn $ "Copying " <> show (unModelName sourceModel) <> " to " <> show (unModelName destModel) <> "..."
  copyRes <- copyModel client sourceModel destModel
  case copyRes of
    Left err -> putStrLn $ "Copy failed: " <> show err
    Right () -> do
      putStrLn "Model copied successfully!"

      putStrLn "\n=== 2. Inspecting Copied Model (showModelWith) ==="
      showRes <- showModelWith client (ShowRequest destModel (Just True))
      case showRes of
        Left err -> putStrLn $ "Show failed: " <> show err
        Right sr -> do
          putStrLn $ "Model architecture family: " <> show (family (srsDetails sr))
          putStrLn $ "Parameter size: " <> show (parameterSize (srsDetails sr))

      putStrLn "\n=== 3. Creating Custom Model with System Prompt (createModel) ==="
      let createReq =
            (defaultCreateRequest customModel)
              { crqFrom = Just sourceModel
              , crqSystem = Just "You are an expert Haskell compiler engineer."
              }
      createRes <- createModel client createReq
      case createRes of
        Left err -> putStrLn $ "Create failed: " <> show err
        Right resp -> do
          putStrLn $ "Create status: " <> show (crsStatus resp)
          _ <- deleteModel client customModel
          putStrLn $ "Custom model " <> show (unModelName customModel) <> " deleted."

      putStrLn "\n=== 4. Checking Blob Existence (checkBlob) ==="
      let testDigest = Digest "sha256:0000000000000000000000000000000000000000000000000000000000000000"
      blobRes <- checkBlob client testDigest
      case blobRes of
        Left err -> putStrLn $ "Blob check returned expected error/status: " <> show err
        Right exists -> putStrLn $ "Dummy blob exists: " <> show exists

      putStrLn "\n=== 5. Deleting Copied Model (deleteModel) ==="
      delRes <- deleteModel client destModel
      case delRes of
        Left err -> putStrLn $ "Delete failed: " <> show err
        Right () -> putStrLn $ "Model " <> show (unModelName destModel) <> " deleted successfully."
