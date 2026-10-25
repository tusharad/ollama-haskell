module Main (main) where

import qualified Data.Text as T
import Ollama

main :: IO ()
main = do
  client <- defaultClient

  putStrLn "=== 1. Ollama Daemon Version ==="
  verRes <- getVersion client
  case verRes of
    Left err -> putStrLn $ "Error fetching version: " <> show err
    Right (Version ver) -> putStrLn $ "Ollama server version: " <> show ver

  putStrLn "\n=== 2. Installed Models (listModels) ==="
  res <- listModels client
  case res of
    Left err -> putStrLn $ "Error listing models: " <> show err
    Right (ListResponse ms) -> do
      putStrLn $ "Found " <> show (length ms) <> " installed models:"
      mapM_ (\m -> putStrLn $ "  - " <> show (unModelName (miName m)) <> " (" <> show (miSize m `div` 1000000) <> " MB)") ms

      case ms of
        (firstModel : _) -> do
          putStrLn $ "\n=== 3. Inspecting Model Details (showModel for " <> show (unModelName (miName firstModel)) <> ") ==="
          showRes <- showModel client (miName firstModel)
          case showRes of
            Left err -> putStrLn $ "Error showing model: " <> show err
            Right sr -> do
              putStrLn $ "Format: " <> show (srsDetails sr)
              putStrLn $ "License length: " <> show (maybe 0 T.length (srsLicense sr))
        [] -> pure ()

  putStrLn "\n=== 4. Running Models in Memory (listRunning / /api/ps) ==="
  psRes <- listRunning client
  case psRes of
    Left err -> putStrLn $ "Error listing running models: " <> show err
    Right (RunningModelsResponse rms) -> do
      putStrLn $ "Currently active models in VRAM: " <> show (length rms)
      mapM_ (\rm -> putStrLn $ "  - " <> show (unModelName (rmName rm)) <> " (VRAM: " <> show (rmSizeVram rm `div` 1000000) <> " MB)") rms
