module Test.Ollama.Property.WebSearch (tests) where

import Data.Aeson (decode, encode)
import Ollama
import Test.Ollama.Property.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "WebSearch Property & Roundtrip QuickCheck Tests"
    [ testProperty "WebSearchRequest JSON roundtrip: ∀ x. decode (encode x) == Just x" $
        \(req :: WebSearchRequest) -> decode (encode req) === Just req
    , testProperty "SearchResult JSON roundtrip: ∀ x. decode (encode x) == Just x" $
        \(res :: SearchResult) -> decode (encode res) === Just res
    , testProperty "WebSearchResponse JSON roundtrip: ∀ x. decode (encode x) == Just x" $
        \(resp :: WebSearchResponse) -> decode (encode resp) === Just resp
    , testProperty "WebFetchRequest JSON roundtrip: ∀ x. decode (encode x) == Just x" $
        \(req :: WebFetchRequest) -> decode (encode req) === Just req
    , testProperty "WebFetchResponse JSON roundtrip: ∀ x. decode (encode x) == Just x" $
        \(resp :: WebFetchResponse) -> decode (encode resp) === Just resp
    ]
