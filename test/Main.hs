module Main (main) where

import Test.Ollama.Golden.Chat qualified as GoldenChat
import Test.Ollama.Golden.Embed qualified as GoldenEmbed
import Test.Ollama.Golden.Generate qualified as GoldenGenerate
import Test.Ollama.Golden.Models qualified as GoldenModels
import Test.Ollama.Property.Roundtrip qualified as PropertyRoundtrip
import Test.Ollama.Unit.Config qualified as UnitConfig
import Test.Ollama.Unit.Error qualified as UnitError
import Test.Ollama.Unit.MCP qualified as UnitMCP
import Test.Ollama.Unit.SchemaBuilder qualified as UnitSchemaBuilder
import Test.Ollama.Unit.Testing qualified as UnitTesting
import Test.Ollama.Unit.Types qualified as UnitTypes
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "ollama-haskell Pure Test Suite"
    [ UnitTypes.tests
    , UnitError.tests
    , UnitConfig.tests
    , UnitSchemaBuilder.tests
    , UnitTesting.testingTests
    , UnitMCP.tests
    , PropertyRoundtrip.tests
    , GoldenChat.tests
    , GoldenGenerate.tests
    , GoldenEmbed.tests
    , GoldenModels.tests
    ]

main :: IO ()
main = defaultMain tests
