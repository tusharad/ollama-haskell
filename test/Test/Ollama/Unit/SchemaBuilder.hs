module Test.Ollama.Unit.SchemaBuilder (tests) where

import Data.Aeson (decode, encode)
import Ollama.Types.Format
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Unit SchemaBuilder DSL Tests"
    [ testCase "Build object schema with required and optional fields" $ do
        let builder =
              emptyObject
                |+ ("name", JString)
                |+ ("age", JInteger)
                |+ ("is_student", JBoolean)
                |! "name"
            sch = buildSchema builder
            encoded = encode sch
        assertBool "Non-empty JSON schema" (not $ null $ show encoded)
    , testCase "Build array schema" $ do
        let itemType = arrayOf JString
            builder = emptyObject |+ ("items", itemType)
            sch = buildSchema builder
            encoded = encode sch
        assertBool "Non-empty array schema" (not $ null $ show encoded)
    , testCase "SchemaFormat wrapping & JSON roundtrip" $ do
        let builder = emptyObject |+ ("count", JInteger) |! "count"
            sch = buildSchema builder
            fmt = SchemaFormat sch
        assertEqual "SchemaFormat JSON roundtrip check" (Just fmt) (decode $ encode fmt)
    ]
