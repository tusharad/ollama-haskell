{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import qualified Test.Ollama.Generate as Generate
import qualified Test.Ollama.Chat as Chat
import Ollama (getVersion, Version(..))
import Data.Text (unpack)

tests :: TestTree
tests =
  testGroup
    "Tests"
    [  Generate.tests
     , Chat.tests
    ]

main :: IO ()
main = do
  eRes <- getVersion
  case eRes of
    Left err -> putStrLn $ show err
    Right (Version r) -> do 
      putStrLn $ "Ollama client version: " <> unpack r
      defaultMain tests
