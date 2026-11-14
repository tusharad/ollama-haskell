module Main (main) where

import Test.Ollama.Golden.Chat qualified as GoldenChat
import Test.Ollama.Golden.Embed qualified as GoldenEmbed
import Test.Ollama.Golden.Generate qualified as GoldenGenerate
import Test.Ollama.Golden.Models qualified as GoldenModels
import Test.Ollama.Property.ModelLifecycle qualified as PropertyModelLifecycle
import Test.Ollama.Property.Roundtrip qualified as PropertyRoundtrip
import Test.Ollama.Property.Streaming qualified as PropertyStreaming
import Test.Ollama.Property.WebSearch qualified as PropertyWebSearch
import Test.Ollama.Unit.Config qualified as UnitConfig
import Test.Ollama.Unit.Error qualified as UnitError
import Test.Ollama.Unit.MCP qualified as UnitMCP
import Test.Ollama.Unit.ModelLifecycle qualified as UnitModelLifecycle
import Test.Ollama.Unit.SchemaBuilder qualified as UnitSchemaBuilder
import Test.Ollama.Unit.SchemaDerive qualified as UnitSchemaDerive
import Test.Ollama.Unit.Streaming qualified as UnitStreaming
import Test.Ollama.Unit.Testing qualified as UnitTesting
import Test.Ollama.Unit.Types qualified as UnitTypes
import Test.Ollama.Unit.WebSearch qualified as UnitWebSearch
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "ollama-haskell Pure Test Suite"
    [ UnitTypes.tests
    , UnitError.tests
    , UnitConfig.tests
    , UnitStreaming.tests
    , UnitWebSearch.tests
    , UnitModelLifecycle.tests
    , UnitSchemaBuilder.tests
    , UnitSchemaDerive.tests
    , UnitTesting.testingTests
    , UnitMCP.tests
    , PropertyRoundtrip.tests
    , PropertyStreaming.tests
    , PropertyWebSearch.tests
    , PropertyModelLifecycle.tests
    , GoldenChat.tests
    , GoldenGenerate.tests
    , GoldenEmbed.tests
    , GoldenModels.tests
    ]

main :: IO ()
main = defaultMain tests
