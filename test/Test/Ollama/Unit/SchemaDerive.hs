module Test.Ollama.Unit.SchemaDerive (tests) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ollama.Types.Format
import Test.Tasty
import Test.Tasty.HUnit

-- ---------------------------------------------------------------------------
-- Test types
-- ---------------------------------------------------------------------------

data SimplePerson = SimplePerson
  { name :: Text
  , age :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToSchema)

data PersonWithOptional = PersonWithOptional
  { personName :: Text
  , personAge :: Int
  , personNickname :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToSchema)

data Address = Address
  { city :: Text
  , zipCode :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToSchema, ToJsonType)

data PersonWithAddress = PersonWithAddress
  { fullName :: Text
  , homeAddress :: Address
  }
  deriving stock (Generic)
  deriving anyclass (ToSchema)

data PersonWithHobbies = PersonWithHobbies
  { hobbyName :: Text
  , hobbies :: [Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToSchema)

data Color = Red | Green | Blue
  deriving stock (Generic)
  deriving anyclass (ToSchema)

data PersonWithMaybeInt = PersonWithMaybeInt
  { pmName :: Text
  , pmScore :: Maybe Int
  }
  deriving stock (Generic)
  deriving anyclass (ToSchema)

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Unit SchemaDerive Tests"
    [ testSimpleRecord
    , testOptionalFields
    , testNestedRecord
    , testArrayFields
    , testEnumType
    , testMaybeUnwrapsType
    , testSchemaForAlias
    , testFormatFor
    , testMatchesManualSchema
    ]

testSimpleRecord :: TestTree
testSimpleRecord = testCase "Simple record produces correct schema" $ do
  let schema = schemaFor @SimplePerson
      props = schemaProperties schema
      req = schemaRequired schema
  assertEqual "Has 'name' property" (Just (Property JString)) (Map.lookup "name" props)
  assertEqual "Has 'age' property" (Just (Property JInteger)) (Map.lookup "age" props)
  assertEqual "Has 2 properties" 2 (Map.size props)
  assertBool "'name' is required" ("name" `elem` req)
  assertBool "'age' is required" ("age" `elem` req)
  assertEqual "2 required fields" 2 (length req)

testOptionalFields :: TestTree
testOptionalFields = testCase "Maybe fields are not required" $ do
  let schema = schemaFor @PersonWithOptional
      props = schemaProperties schema
      req = schemaRequired schema
  assertEqual "Has 3 properties" 3 (Map.size props)
  assertEqual
    "'personNickname' maps to JString"
    (Just (Property JString))
    (Map.lookup "personNickname" props)
  assertBool "'personName' is required" ("personName" `elem` req)
  assertBool "'personAge' is required" ("personAge" `elem` req)
  assertBool "'personNickname' is NOT required" ("personNickname" `notElem` req)
  assertEqual "2 required fields" 2 (length req)

testNestedRecord :: TestTree
testNestedRecord = testCase "Nested record becomes JObject" $ do
  let schema = schemaFor @PersonWithAddress
      props = schemaProperties schema
  case Map.lookup "homeAddress" props of
    Just (Property (JObject innerSchema)) -> do
      let innerProps = schemaProperties innerSchema
      assertEqual "Inner has 'city'" (Just (Property JString)) (Map.lookup "city" innerProps)
      assertEqual "Inner has 'zipCode'" (Just (Property JString)) (Map.lookup "zipCode" innerProps)
    other -> assertFailure $ "Expected JObject for homeAddress, got: " <> show other

testArrayFields :: TestTree
testArrayFields = testCase "List fields become JArray" $ do
  let schema = schemaFor @PersonWithHobbies
      props = schemaProperties schema
  assertEqual
    "'hobbies' maps to JArray JString"
    (Just (Property (JArray JString)))
    (Map.lookup "hobbies" props)

testEnumType :: TestTree
testEnumType = testCase "Simple enum produces schema with constructor names" $ do
  let schema = schemaFor @Color
      req = schemaRequired schema
  -- Enum constructors are stored in required list (as enum values)
  assertBool "Contains 'red'" ("red" `elem` req)
  assertBool "Contains 'green'" ("green" `elem` req)
  assertBool "Contains 'blue'" ("blue" `elem` req)
  assertEqual "3 enum values" 3 (length req)
  assertEqual "No properties" 0 (Map.size $ schemaProperties schema)

testMaybeUnwrapsType :: TestTree
testMaybeUnwrapsType = testCase "Maybe Int field maps to JInteger" $ do
  let schema = schemaFor @PersonWithMaybeInt
      props = schemaProperties schema
  assertEqual "'pmScore' maps to JInteger" (Just (Property JInteger)) (Map.lookup "pmScore" props)

testSchemaForAlias :: TestTree
testSchemaForAlias = testCase "schemaFor is equivalent to toSchema" $ do
  let s1 = schemaFor @SimplePerson
      s2 = toSchema @SimplePerson
  assertEqual "schemaFor == toSchema" s1 s2

testFormatFor :: TestTree
testFormatFor = testCase "formatFor wraps in SchemaFormat" $ do
  let fmt = formatFor @SimplePerson
      expected = SchemaFormat (schemaFor @SimplePerson)
  assertEqual "formatFor wraps schema" expected fmt

testMatchesManualSchema :: TestTree
testMatchesManualSchema = testCase "Derived schema matches manual DSL schema" $ do
  let derived = schemaFor @SimplePerson
      manual =
        buildSchema $
          emptyObject
            |+ ("name", JString)
            |+ ("age", JInteger)
            |!! ["name", "age"]
  assertEqual "Properties match" (schemaProperties manual) (schemaProperties derived)
  -- Required fields should contain the same elements (order may differ)
  let derivedReq = schemaRequired derived
      manualReq = schemaRequired manual
  assertBool "All derived required fields in manual" (all (`elem` manualReq) derivedReq)
  assertBool "All manual required fields in derived" (all (`elem` derivedReq) manualReq)
