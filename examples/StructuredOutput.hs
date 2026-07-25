module Main (main) where

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
  let req =
        (generateRequest "llama3.2" "Generate a person profile.")
          { genFormat = Just (SchemaFormat personSchema)
          }
  res <- generate client req
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp -> putStrLn $ "Structured Response:\n" <> grResponse resp
