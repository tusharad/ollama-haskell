module Main (main) where

import Control.Exception (try)
import Ollama

main :: IO ()
main = do
  putStrLn "=== Ollama Client Lifecycle, Configuration & Error Handling ==="

  putStrLn "\n1. Client from environment variables (clientFromEnv)..."
  envClient <- clientFromEnv
  verRes <- getVersion envClient
  case verRes of
    Left err -> putStrLn $ "Error: " <> show err
    Right (Version v) -> putStrLn $ "Connected via env client: v" <> show v
  closeClient envClient
  putStrLn "envClient successfully closed with closeClient."

  putStrLn "\n2. Scoped resource bracket (withClient) with custom config..."
  let customCfg =
        defaultConfig
          { configBaseUrl = "http://localhost:11434"
          , configRetry = ExponentialRetry 3 1
          , configTimeout = 15
          , configLogger = Just (\lvl msg -> putStrLn $ "[" <> show lvl <> "] " <> show msg)
          , configHeaders = [("User-Agent", "ollama-haskell-demo/1.0")]
          }

  withClient customCfg $ \client -> do
    putStrLn "Inside withClient block (auto-cleanup guaranteed):"
    modelsRes <- listModels client
    case modelsRes of
      Left err -> putStrLn $ "Error: " <> show err
      Right (ListResponse ms) -> putStrLn $ "Successfully listed " <> show (length ms) <> " models."

  putStrLn "\n3. Testing error classification and isRetryable..."
  let timeoutErr = TimeoutError
      apiErr500 = ApiError 500 "Internal Server Error"
      apiErr400 = ApiError 400 "Bad Request"
  putStrLn $ "Is TimeoutError retryable? " <> show (isRetryable timeoutErr)
  putStrLn $ "Is HTTP 500 retryable? " <> show (isRetryable apiErr500)
  putStrLn $ "Is HTTP 400 retryable? " <> show (isRetryable apiErr400)

  putStrLn "\n4. Testing throwOllama exception raising..."
  caught <- try (throwOllama (InvalidRequest "Simulated invalid parameter"))
  case caught of
    Left (err :: OllamaError) -> putStrLn $ "Successfully caught thrown OllamaError: " <> show err
    Right () -> putStrLn "Error was not caught!"

  putStrLn "\n=== Client Lifecycle Showcase Completed Successfully ==="
