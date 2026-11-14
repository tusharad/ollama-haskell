module Main (main) where

import Data.Text qualified as T
import Ollama

main :: IO ()
main = do
  putStrLn "=== Ollama GPU VRAM & Model Lifecycle Example ==="
  client <- defaultClient
  let targetModel = ModelName "qwen3.5:2b"

  putStrLn $ "\n1. Checking if " <> show (unModelName targetModel) <> " is currently running..."
  runningBefore <- isModelRunning client targetModel
  case runningBefore of
    Left err -> putStrLn $ "Error checking status: " <> show err
    Right isRun -> putStrLn $ "Is model running? " <> show isRun

  putStrLn "\n2. Preloading model into VRAM (preloadModel)..."
  preloadRes <- preloadModel client targetModel (Just (Duration (10 * 60 * 1000000000)))
  case preloadRes of
    Left err -> putStrLn $ "Preload error: " <> show err
    Right () -> putStrLn "Model successfully loaded into VRAM."

  putStrLn "\n3. Inspecting active VRAM footprint (getModelVramUsage)..."
  vramRes <- getModelVramUsage client targetModel
  case vramRes of
    Left err -> putStrLn $ "Error getting VRAM: " <> show err
    Right Nothing -> putStrLn "Model is not active in VRAM."
    Right (Just bytes) ->
      putStrLn $ "Model VRAM usage: " <> show (bytes `div` (1024 * 1024)) <> " MB"

  putStrLn "\n4. Listing all currently active models (/api/ps)..."
  psRes <- listRunning client
  case psRes of
    Left err -> putStrLn $ "Error listing processes: " <> show err
    Right (RunningModelsResponse rms) -> do
      putStrLn $ "Total resident models: " <> show (length rms)
      mapM_
        ( \rm ->
            putStrLn $
              "  - "
                <> T.unpack (unModelName (rmName rm))
                <> " | VRAM: "
                <> show (rmSizeVram rm `div` (1024 * 1024))
                <> " MB | Expires at: "
                <> show (rmExpiresAt rm)
        )
        rms

  putStrLn "\n5. Evicting model from VRAM (unloadModel)..."
  unloadRes <- unloadModel client targetModel
  case unloadRes of
    Left err -> putStrLn $ "Unload error: " <> show err
    Right () -> putStrLn "Model successfully unloaded and VRAM freed."

  runningAfter <- isModelRunning client targetModel
  case runningAfter of
    Left err -> putStrLn $ "Error: " <> show err
    Right isRun -> putStrLn $ "Is model still resident? " <> show isRun
