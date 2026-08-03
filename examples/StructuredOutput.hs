module Main (main) where

import Data.Text.IO qualified as TIO
import Ollama
import Ollama.Types.Format.SchemaBuilder

personSchema :: Schema
personSchema =
  buildSchema $
    emptyObject
      |+ ("name", JString)
      |+ ("age", JInteger)
      |! "name"

main :: IO ()
main = do
  client <- defaultClient
  let opts = Just (defaultOptions {optNumPredict = Just 20})
      req =
        (generateRequest "qwen3.5:2b" "Generate a person profile.")
          { genFormat = Just (SchemaFormat personSchema)
          , genOptions = opts
          , genThink = Just ThinkDisabled
          }
  res <- generate client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> TIO.putStrLn $ "Structured Response:\n" <> grResponse resp
