{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Ollama.Types.Format.SchemaDerive
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : experimental
Portability : portable

Generic derivation of JSON 'Schema' from Haskell record types.

This module provides the 'ToSchema' typeclass, which can automatically
derive a JSON Schema ('Schema') from any Haskell data type that has a
'GHC.Generics.Generic' instance.  This eliminates the need to manually
construct schemas using the 'SchemaBuilder' DSL when working with
Ollama's structured output API.

== Usage

Simply derive 'Generic' and declare a 'ToSchema' instance:

@
data Person = Person
  { name :: Text
  , age  :: Int
  } deriving stock ('GHC.Generics.Generic', Show)
    deriving anyclass ('ToSchema')

\-\- Use it:
\-\- >>> 'schemaFor' \@Person
\-\- >>> 'formatFor' \@Person
@

== Supported types

* __Record types__: All fields become properties; non‑'Maybe' fields
  are marked as required.
* __'Maybe' fields__: Present in the schema properties but excluded
  from the @required@ array.
* __Nested records__: Recursively derived as nested @object@ schemas.
* __Lists__: Mapped to @array@ schemas.
* __Simple enums__: Sum types with all nullary constructors are
  represented as @{\"type\": \"string\", \"enum\": [...]}@.

@since 0.4.0.0
-}
module Ollama.Types.Format.SchemaDerive (
  -- * Typeclass
  ToSchema (..),
  ToJsonType (..),

  -- * Convenience functions
  schemaFor,
) where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Ollama.Types.Format.SchemaBuilder (JsonType (..), Property (..), Schema (..))

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

{- | Typeclass for types whose structure can be represented as a JSON 'Schema'.

A default implementation is provided via @GHC.Generics@, so you can derive
it for any record type that has a 'Generic' instance:

@
data MyType = MyType { field1 :: Text, field2 :: Int }
  deriving stock ('Generic')
  deriving anyclass ('ToSchema')
@

@since 0.4.0.0
-}
class ToSchema a where
  {- | Produce the JSON 'Schema' for type @a@.

  @since 0.4.0.0
  -}
  toSchema :: Schema
  default toSchema :: (GToSchema (Rep a)) => Schema
  toSchema = gToSchema @(Rep a)

{- | Convenience alias for @'toSchema' \@a@.

@since 0.4.0.0
-}
schemaFor :: forall a. (ToSchema a) => Schema
schemaFor = toSchema @a

{- | Map a Haskell type to its JSON Schema 'JsonType'.

Instances are provided for common primitive types ('Text', 'Int', 'Bool',
'Double', etc.), 'Maybe', lists, and any type with a 'ToSchema' instance
(which maps to a nested @object@ schema).

For nested record types, you don't need to write an instance manually —
simply ensure the nested type has a 'ToSchema' instance and the default
method will handle it:

@
data Address = Address { city :: Text, zip :: Text }
  deriving stock ('Generic')
  deriving anyclass ('ToSchema', 'ToJsonType')
@

@since 0.4.0.0
-}
class ToJsonType a where
  toJsonType :: JsonType
  default toJsonType :: (ToSchema a) => JsonType
  toJsonType = JObject (toSchema @a)

-- ---------------------------------------------------------------------------
-- GHC.Generics machinery (internal)
-- ---------------------------------------------------------------------------

-- | Walk a generic representation to produce a 'Schema'.
class GToSchema (f :: Type -> Type) where
  gToSchema :: Schema

-- | Collect fields (properties + required list) from a generic representation.
class GCollectFields (f :: Type -> Type) where
  gCollectFields :: ([(Text, Property)], [Text])

-- | Collect constructor names from a sum type for enum schemas.
class GEnumConstructors (f :: Type -> Type) where
  gEnumConstructors :: [Text]

-- ---------------------------------------------------------------------------
-- Datatype / Constructor / Sum dispatch
-- ---------------------------------------------------------------------------

-- | Strip datatype metadata wrapper.
instance (GToSchemaDispatch f) => GToSchema (M1 D meta f) where
  gToSchema = gToSchemaDispatch @f

-- | Dispatch: determine if this is a single-constructor record or a sum type.
class GToSchemaDispatch (f :: Type -> Type) where
  gToSchemaDispatch :: Schema

-- | Single constructor → collect fields into an object schema.
instance (GCollectFields f) => GToSchemaDispatch (M1 C meta f) where
  gToSchemaDispatch =
    let (props, req) = gCollectFields @f
     in Schema (Map.fromList props) req

{- | Sum type → try to build an enum schema.
We treat all-nullary sum types as string enums.
-}
instance (GEnumConstructors f, GEnumConstructors g) => GToSchemaDispatch (f :+: g) where
  gToSchemaDispatch =
    let constructors = gEnumConstructors @f <> gEnumConstructors @g
     in Schema Map.empty constructors

-- ---------------------------------------------------------------------------
-- Constructor / field collection
-- ---------------------------------------------------------------------------

-- | Strip constructor metadata wrapper.
instance (GCollectFields f) => GCollectFields (M1 C meta f) where
  gCollectFields = gCollectFields @f

-- | Product of fields: combine both sides.
instance (GCollectFields f, GCollectFields g) => GCollectFields (f :*: g) where
  gCollectFields =
    let (ps1, rs1) = gCollectFields @f
        (ps2, rs2) = gCollectFields @g
     in (ps1 <> ps2, rs1 <> rs2)

-- | Unit constructor (no fields).
instance GCollectFields U1 where
  gCollectFields = ([], [])

-- | Record field with a 'Maybe' type (optional — not required).
instance
  {-# OVERLAPPING #-}
  (KnownSymbol name, ToJsonType a) =>
  GCollectFields (M1 S ('MetaSel ('Just name) su ss ds) (K1 R (Maybe a)))
  where
  gCollectFields =
    let fieldName = T.pack $ symbolVal (Proxy @name)
        prop = Property (toJsonType @a)
     in ([(fieldName, prop)], [])

-- | Record field with a non-'Maybe' type (required).
instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol name, ToJsonType a) =>
  GCollectFields (M1 S ('MetaSel ('Just name) su ss ds) (K1 R a))
  where
  gCollectFields =
    let fieldName = T.pack $ symbolVal (Proxy @name)
        prop = Property (toJsonType @a)
     in ([(fieldName, prop)], [fieldName])

-- ---------------------------------------------------------------------------
-- ToJsonType instances
-- ---------------------------------------------------------------------------

-- Strings
instance ToJsonType Text where toJsonType = JString
instance ToJsonType String where toJsonType = JString

-- Integers
instance ToJsonType Int where toJsonType = JInteger
instance ToJsonType Int8 where toJsonType = JInteger
instance ToJsonType Int16 where toJsonType = JInteger
instance ToJsonType Int32 where toJsonType = JInteger
instance ToJsonType Int64 where toJsonType = JInteger
instance ToJsonType Integer where toJsonType = JInteger
instance ToJsonType Word where toJsonType = JInteger
instance ToJsonType Word8 where toJsonType = JInteger
instance ToJsonType Word16 where toJsonType = JInteger
instance ToJsonType Word32 where toJsonType = JInteger
instance ToJsonType Word64 where toJsonType = JInteger

-- Floating point
instance ToJsonType Double where toJsonType = JNumber
instance ToJsonType Float where toJsonType = JNumber

-- Boolean
instance ToJsonType Bool where toJsonType = JBoolean

-- Maybe: unwrap to the inner type
instance (ToJsonType a) => ToJsonType (Maybe a) where
  toJsonType = toJsonType @a

-- Lists / arrays
instance (ToJsonType a) => ToJsonType [a] where
  toJsonType = JArray (toJsonType @a)

-- ---------------------------------------------------------------------------
-- Enum constructors
-- ---------------------------------------------------------------------------

-- | Sum of two branches.
instance (GEnumConstructors f, GEnumConstructors g) => GEnumConstructors (f :+: g) where
  gEnumConstructors = gEnumConstructors @f <> gEnumConstructors @g

-- | A nullary constructor (unit): extract constructor name.
instance (KnownSymbol name) => GEnumConstructors (M1 C ('MetaCons name fx 'False) U1) where
  gEnumConstructors = [T.toLower . T.pack $ symbolVal (Proxy @name)]

{- | A constructor with fields — not a simple enum. Produce a type error
at the instance level by not providing an instance (compile-time failure).
Users will get "No instance for GEnumConstructors ..." which is clear enough.
-}
