{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (unpack)
import Ollama (Version (..), getVersion)
import Test.Ollama.Chat qualified as Chat
import Test.Ollama.Embedding qualified as Embeddings
import Test.Ollama.Generate qualified as Generate
import Test.Ollama.Show qualified as Show
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Generate.tests
    , Chat.tests
    , Show.tests
    , Embeddings.tests
    ]

main :: IO ()
main = do
  eRes <- getVersion
  case eRes of
    Left err -> putStrLn $ show err
    Right (Version r) -> do
      putStrLn $ "Ollama client version: " <> unpack r
      defaultMain tests
