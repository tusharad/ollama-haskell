module Main (main) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import Ollama

main :: IO ()
main = do
  putStrLn "=== Offline Testing with withMockClient & Mock Fixtures ==="

  putStrLn "\n1. Mocking /api/chat with mockChatResponse..."
  let chatFixture = mockChatResponse "mock-model:latest" "Hello from pure unit test!"
      chatBytes = BSL.toStrict (encode chatFixture)

  withMockClient chatBytes $ \client -> do
    res <- chat client (chatRequest "mock-model:latest" (userMessage "Hi!" :| []))
    case res of
      Left err -> putStrLn $ "Error: " <> show err
      Right resp -> case crMessage resp of
        Just msg -> putStrLn $ "Mock Chat Response: " <> T.unpack (messageContent msg)
        Nothing -> putStrLn "No message returned."

  putStrLn "\n2. Mocking /api/generate with mockGenerateResponse..."
  let genFixture = mockGenerateResponse "mock-model:latest" "Generated text without Ollama server."
      genBytes = BSL.toStrict (encode genFixture)

  withMockClient genBytes $ \client -> do
    res <- generate client (generateRequest "mock-model:latest" "Test prompt")
    case res of
      Left err -> putStrLn $ "Error: " <> show err
      Right resp -> putStrLn $ "Mock Generate Response: " <> T.unpack (grResponse resp)

  putStrLn "\n3. Mocking /api/embed with mockEmbedResponse..."
  let embedFixture = mockEmbedResponse "mock-model:latest" [[0.1, 0.2, 0.3], [0.4, 0.5, 0.6]]
      embedBytes = BSL.toStrict (encode embedFixture)

  withMockClient embedBytes $ \client -> do
    res <- embed client (embedRequest "mock-model:latest" ["text 1", "text 2"])
    case res of
      Left err -> putStrLn $ "Error: " <> show err
      Right resp -> putStrLn $ "Mock Embeddings count: " <> show (length (erEmbeddings resp))

  putStrLn "\n4. Mocking /api/tags with mockListModelsResponse..."
  let listFixture = mockListModelsResponse ["mock-llama:latest", "mock-gemma:latest"]
      listBytes = BSL.toStrict (encode listFixture)

  withMockClient listBytes $ \client -> do
    res <- listModels client
    case res of
      Left err -> putStrLn $ "Error: " <> show err
      Right (ListResponse ms) -> do
        putStrLn $ "Mock Models count: " <> show (length ms)
        mapM_ (\m -> putStrLn $ "  - " <> show (unModelName (miName m))) ms

  putStrLn "\n=== Mock Testing Showcase Completed Successfully ==="
