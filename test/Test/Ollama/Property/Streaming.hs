module Test.Ollama.Property.Streaming (tests) where

import Data.Maybe (fromMaybe)
import Ollama
import Test.Ollama.Property.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Streaming Property & Algebraic Law Tests"
    [ testProperty
        "accumulateChat associativity: ∀ a b c. accumulateChat (accumulateChat a b) c == accumulateChat a (accumulateChat b c)"
        $ \(a :: ChatResponse) (b :: ChatResponse) (c :: ChatResponse) ->
          let lhs = accumulateChat (accumulateChat a b) c
              rhs = accumulateChat a (accumulateChat b c)
              -- Content check
              msgContent res = maybe "" messageContent (crMessage res)
              msgThinking res = fromMaybe "" (messageThinking =<< crMessage res)
           in (msgContent lhs === msgContent rhs)
                .&&. (msgThinking lhs === msgThinking rhs)
                .&&. (crDone lhs === crDone rhs)
    , testProperty
        "accumulateGenerate associativity: ∀ a b c. accumulateGenerate (accumulateGenerate a b) c == accumulateGenerate a (accumulateGenerate b c)"
        $ \(a :: GenerateResponse) (b :: GenerateResponse) (c :: GenerateResponse) ->
          let lhs = accumulateGenerate (accumulateGenerate a b) c
              rhs = accumulateGenerate a (accumulateGenerate b c)
              genThinking res = fromMaybe "" (grThinking res)
           in (grResponse lhs === grResponse rhs)
                .&&. (genThinking lhs === genThinking rhs)
                .&&. (grDone lhs === grDone rhs)
    , testProperty
        "accumulateChatChunks content conservation: ∀ c1 c2. content (acc [c1, c2]) == content c1 <> content c2"
        $ \(c1 :: ChatResponse) (c2 :: ChatResponse) ->
          case accumulateChatChunks [c1, c2] of
            Left _ -> property False
            Right acc ->
              let expected = maybe "" messageContent (crMessage c1) <> maybe "" messageContent (crMessage c2)
                  actual = maybe "" messageContent (crMessage acc)
               in actual === expected
    , testProperty
        "accumulateGenerateChunks content conservation: ∀ g1 g2. response (acc [g1, g2]) == response g1 <> response g2"
        $ \(g1 :: GenerateResponse) (g2 :: GenerateResponse) ->
          case accumulateGenerateChunks [g1, g2] of
            Left _ -> property False
            Right acc ->
              grResponse acc === (grResponse g1 <> grResponse g2)
    ]
