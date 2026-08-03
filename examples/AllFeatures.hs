{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Comprehensive example demonstrating all v1.0 features of ollama-haskell:
  * Client builder with env resolution, custom headers, & retry policies
  * Conduit-based response streaming
  * Thinking / reasoning model integration
  * Function / tool calling
  * Structured JSON schema output
  * Vector embeddings & token throughput metrics
  * Transactional STM conversation storage

@since 1.0.0.0
-}
module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Ollama

fastOptions :: Maybe ModelOptions
fastOptions = Just (defaultOptions {optNumPredict = Just 20})

main :: IO ()
main = do
  putStrLn "=========================================================="
  putStrLn "   ollama-haskell v1.0 — All Features Showcase Example"
  putStrLn "=========================================================="

  -- 1. Client Builder with env vars, custom headers, & Exponential Retry Policy
  putStrLn "\n[1] Initializing client..."
  let customConfig =
        defaultConfig
          { configRetry = ExponentialRetry 3 1
          , configHeaders = [("X-Custom-Client", "ollama-haskell-v1.0")]
          }
  client <- newClient customConfig
  putStrLn "Client initialized with ExponentialRetry policy."

  -- 2. System Inspection
  putStrLn "\n[2] System Engine Version..."
  verRes <- getVersion client
  case verRes of
    Left err -> putStrLn $ "Error fetching version: " <> show err
    Right ver -> putStrLn $ "Ollama Engine Version: " <> T.unpack (unVersion ver)

  putStrLn "\n[3] Listing Local Models..."
  modelsRes <- listModels client
  case modelsRes of
    Left err -> putStrLn $ "Error listing models: " <> show err
    Right (ListResponse ms) -> do
      putStrLn $ "Found " <> show (length ms) <> " installed models:"
      mapM_ (\m -> putStrLn $ "  - " <> T.unpack (unModelName (miName m))) ms

  let model = "qwen3.5:2b"

  -- 4. Conduit-Based Streaming Chat
  putStrLn $ "\n[4] Streaming Chat with Conduit (" <> T.unpack (unModelName model) <> ")..."
  let streamReq =
        (chatRequest model (userMessage "Count from 1 to 5." :| []))
          { chatOptions = fastOptions
          , chatThink = Just ThinkDisabled
          }
  putStr "Response stream: "
  chunks <- collectStream (chatStream client streamReq)
  mapM_
    ( \c -> case crMessage c of
        Just msg -> putStr (T.unpack (messageContent msg))
        Nothing -> pure ()
    )
    chunks
  putStrLn ""

  -- 5. Non-Streaming Chat with Token Throughput Metrics
  putStrLn "\n[5] Non-Streaming Chat & Token Metrics..."
  let chatReq =
        ( chatRequest
            model
            (systemMessage "You are a concise assistant." :| [userMessage "Explain gravity in one sentence."])
        )
          { chatOptions = fastOptions
          , chatThink = Just ThinkDisabled
          }
  chatRes <- chat client chatReq
  case chatRes of
    Left err -> putStrLn $ "Chat error: " <> show err
    Right resp -> do
      case crMessage resp of
        Just msg -> putStrLn $ "Answer: " <> T.unpack (messageContent msg)
        Nothing -> putStrLn "No message returned."
      case chatEvalTokensPerSecond resp of
        Just tps -> putStrLn $ "Generation Speed: " <> show tps <> " tokens/sec"
        Nothing -> pure ()

  -- 6. Thinking / Reasoning Model Integration
  putStrLn "\n[6] Generation with Thinking Mode..."
  let thinkReq =
        (generateRequest model "What is 15 * 14?")
          { genThink = Just (ThinkLevel ThinkMedium)
          , genOptions = fastOptions
          }
  thinkRes <- generate client thinkReq
  case thinkRes of
    Left err -> putStrLn $ "Generate error: " <> show err
    Right resp -> putStrLn $ "Result: " <> T.unpack (grResponse resp)

  -- 7. Tool / Function Calling
  putStrLn "\n[7] Tool / Function Calling..."
  let calcTool =
        Tool
          { toolType = "function"
          , toolFunction =
              FunctionDef
                { fnName = "calculator"
                , fnDescription = Just "Perform basic math calculations"
                , fnParameters =
                    Just
                      FunctionParameters
                        { fpType = "object"
                        , fpProperties = Nothing
                        , fpRequired = Just ["expression"]
                        , fpAdditionalProperties = Nothing
                        , fpDescription = Nothing
                        , fpEnum = Nothing
                        }
                , fnStrict = Just True
                }
          }
      toolReq =
        (chatRequest model (userMessage "Calculate 42 * 8" :| []))
          { chatTools = Just [calcTool]
          , chatOptions = fastOptions
          , chatThink = Just ThinkDisabled
          }
  toolRes <- chat client toolReq
  case toolRes of
    Left err -> putStrLn $ "Tool chat error: " <> show err
    Right resp -> case crMessage resp of
      Just msg -> case messageToolCalls msg of
        Just calls -> putStrLn $ "Model requested tool execution: " <> show calls
        Nothing -> putStrLn $ "Response: " <> T.unpack (messageContent msg)
      Nothing -> putStrLn "No message returned."

  -- 8. Vector Embeddings
  putStrLn "\n[8] Vector Embeddings..."
  let embReq = embedRequest model ["Haskell AI development", "Ollama LLM client"]
  embRes <- embed client embReq
  case embRes of
    Left err -> putStrLn $ "Embed error: " <> show err
    Right resp -> putStrLn $ "Generated " <> show (length (erEmbeddings resp)) <> " embedding vectors."

  -- 9. Transactional Conversation Store
  putStrLn "\n[9] STM Conversation Store..."
  store <- initInMemoryStore
  now <- getCurrentTime
  let convId = "demo-session-42"
      session =
        Conversation
          convId
          [systemMessage "Context saved.", userMessage "Favorite language is Haskell."]
          model
          now
          now
  saveConversationInMemory store session
  retrieved <- loadConversationInMemory store convId
  case retrieved of
    Just c ->
      putStrLn $
        "Successfully loaded conversation ["
          <> T.unpack (conversationId c)
          <> "] with "
          <> show (length (messages c))
          <> " messages."
    Nothing -> putStrLn "Failed to load conversation."

  closeClient client
  putStrLn "\n=========================================================="
  putStrLn "   Showcase Completed Successfully!"
  putStrLn "=========================================================="
