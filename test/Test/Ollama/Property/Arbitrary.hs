{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ollama.Property.Arbitrary () where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..))
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
  arbitrary =
    oneof
      [ elements [ThinkLow, ThinkMedium, ThinkHigh, ThinkMax]
      , ThinkCustom <$> elements ["xhigh", "deep", "ultra"]
      ]

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
      <*> pure Nothing

instance Arbitrary Format where
  arbitrary = pure JsonFormat

instance Arbitrary UTCTime where
  arbitrary = pure (UTCTime (toEnum 59000) 0)

instance Arbitrary ToolCallFunction where
  arbitrary = ToolCallFunction <$> genNonEmptyText <*> pure mempty

instance Arbitrary ToolCall where
  arbitrary = ToolCall <$> arbitrary

instance Arbitrary TokenLogprob where
  arbitrary = TokenLogprob <$> genNonEmptyText <*> choose (-10.0, 0.0)

instance Arbitrary Logprob where
  arbitrary = Logprob <$> genNonEmptyText <*> choose (-10.0, 0.0) <*> pure Nothing

instance Arbitrary ChatResponse where
  arbitrary = do
    model <- arbitrary
    time <- arbitrary
    msg <- arbitrary
    done <- arbitrary
    doneReason <- oneof [pure Nothing, Just <$> genNonEmptyText]
    totalDur <- arbitrary
    loadDur <- arbitrary
    promptCount <- arbitrary
    promptDur <- arbitrary
    evalCount <- arbitrary
    evalDur <- arbitrary
    pure $
      ChatResponse
        { crModel = model
        , crCreatedAt = time
        , crMessage = Just msg
        , crDone = done
        , crDoneReason = doneReason
        , crTotalDuration = totalDur
        , crLoadDuration = loadDur
        , crPromptEvalCount = promptCount
        , crPromptEvalDuration = promptDur
        , crEvalCount = evalCount
        , crEvalDuration = evalDur
        , crLogprobs = Nothing
        }

instance Arbitrary GenerateResponse where
  arbitrary = do
    model <- arbitrary
    time <- arbitrary
    resp <- genNonEmptyText
    done <- arbitrary
    doneReason <- oneof [pure Nothing, Just <$> genNonEmptyText]
    context <- arbitrary
    totalDur <- arbitrary
    loadDur <- arbitrary
    promptCount <- arbitrary
    promptDur <- arbitrary
    evalCount <- arbitrary
    evalDur <- arbitrary
    thinking <- oneof [pure Nothing, Just <$> genNonEmptyText]
    pure $
      GenerateResponse
        { grModel = model
        , grCreatedAt = time
        , grResponse = resp
        , grDone = done
        , grDoneReason = doneReason
        , grContext = context
        , grTotalDuration = totalDur
        , grLoadDuration = loadDur
        , grPromptEvalCount = promptCount
        , grPromptEvalDuration = promptDur
        , grEvalCount = evalCount
        , grEvalDuration = evalDur
        , grThinking = thinking
        , grImage = Nothing
        , grLogprobs = Nothing
        , grCompleted = Nothing
        , grTotal = Nothing
        }

instance Arbitrary WebSearchRequest where
  arbitrary = WebSearchRequest <$> genNonEmptyText <*> arbitrary

instance Arbitrary SearchResult where
  arbitrary = SearchResult <$> genNonEmptyText <*> genNonEmptyText <*> genNonEmptyText

instance Arbitrary WebSearchResponse where
  arbitrary = WebSearchResponse <$> arbitrary

instance Arbitrary WebFetchRequest where
  arbitrary = WebFetchRequest <$> genNonEmptyText

instance Arbitrary WebFetchResponse where
  arbitrary = WebFetchResponse <$> genNonEmptyText <*> genNonEmptyText
