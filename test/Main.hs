module Main (main) where

import Test.Ollama.Golden.Chat qualified as GoldenChat
import Test.Ollama.Property.Roundtrip qualified as PropertyRoundtrip
import Test.Ollama.Unit.Types qualified as UnitTypes
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "ollama-haskell Pure Test Suite"
    [ UnitTypes.tests
    , PropertyRoundtrip.tests
    , GoldenChat.tests
    ]

main :: IO ()
main = defaultMain tests
