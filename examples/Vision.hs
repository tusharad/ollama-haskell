module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text.IO as TIO
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  putStrLn "=== Multimodal / Vision API with Local Image ==="
  putStrLn "Reading examples/sample.png using readImageBase64..."
  img <- readImageBase64 "examples/sample.png"
  putStrLn "Image loaded and encoded successfully."

  putStrLn "\n--- 1. Multimodal Chat (gemma3:latest) ---"
  let chatMsg = imageMessage "What colors and shapes do you see in this image? Keep it to 2 sentences." [img]
      opts = Just (defaultOptions {optNumPredict = Just 60})
      chatReq =
        (chatRequest "gemma3:latest" (chatMsg :| []))
          { chatOptions = opts
          }

  chatRes <- chat client chatReq
  case chatRes of
    Left err -> putStrLn $ "Vision chat error: " <> show err
    Right resp -> case crMessage resp of
      Just msg -> do
        putStrLn "Model Description:"
        TIO.putStrLn (messageFullText msg)
      Nothing -> putStrLn "No message returned."

  putStrLn "\n--- 2. Multimodal Generation (generate endpoint with genImages) ---"
  let genReq =
        (generateRequest "gemma3:latest" "Identify what kind of asset or diagram this is in one line.")
          { genImages = Just [img]
          , genOptions = opts
          }
  genRes <- generate client genReq
  case genRes of
    Left err -> putStrLn $ "Vision generate error: " <> show err
    Right resp -> do
      putStrLn "Generated Answer:"
      TIO.putStrLn (generateFullText resp)
