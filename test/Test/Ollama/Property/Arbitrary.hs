{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ollama.Property.Arbitrary () where

import Data.Text (Text)
import Data.Text qualified as T
import Ollama
import Ollama.Types.Message qualified as M
import Test.QuickCheck

genNonEmptyText :: Gen Text
genNonEmptyText = T.pack <$> listOf1 (elements ['a' .. 'z'])

instance Arbitrary Role where
  arbitrary = elements [System, User, Assistant, M.Tool]

instance Arbitrary ModelName where
  arbitrary = ModelName <$> genNonEmptyText

instance Arbitrary Digest where
  arbitrary = Digest . ("sha256:" <>) <$> genNonEmptyText

instance Arbitrary Base64Image where
  arbitrary = Base64Image <$> genNonEmptyText

instance Arbitrary Duration where
  arbitrary = Duration <$> choose (1, 1000000000)

instance Arbitrary ThinkingLevel where
  arbitrary = elements [ThinkLow, ThinkMedium, ThinkHigh, ThinkMax]

instance Arbitrary Think where
  arbitrary =
    oneof
      [ pure ThinkEnabled
      , pure ThinkDisabled
      , ThinkLevel <$> arbitrary
      ]

instance Arbitrary Message where
  arbitrary =
    Message
      <$> arbitrary
      <*> genNonEmptyText
      <*> pure Nothing
      <*> pure Nothing
      <*> pure Nothing
      <*> pure Nothing

instance Arbitrary Format where
  arbitrary = pure JsonFormat
