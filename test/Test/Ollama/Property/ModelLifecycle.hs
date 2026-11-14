module Test.Ollama.Property.ModelLifecycle (tests) where

import Data.Aeson (decode, encode)
import Data.Text qualified as T
import Ollama
import Test.Ollama.Property.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Model Lifecycle Property & Algebraic Law Tests"
    [ testProperty "RunningModelsResponse JSON roundtrip: ∀ x. decode (encode x) == Just x" $
        \(resp :: RunningModelsResponse) ->
          decode (encode resp) === Just resp
    , testProperty
        "findRunningModel soundness: ∀ ms target rm. findRunningModel target ms == Just rm ==> matchesRunningModel target rm"
        $ \(models :: [RunningModel]) (target :: ModelName) ->
          case findRunningModel target models of
            Nothing -> property True
            Just rm -> matchesRunningModel target rm === True
    , testProperty "matchesRunningModel reflexivity: ∀ rm. matchesRunningModel (rmName rm) rm" $
        \(rm :: RunningModel) ->
          matchesRunningModel (rmName rm) rm === True
    , testProperty
        "Tag equivalence law: ∀ base models. without colon ==> find (ModelName base) == find (ModelName (base <> \":latest\"))"
        $ \(models :: [RunningModel]) (target :: ModelName) ->
          let cleanBase = T.filter (/= ':') (unModelName target)
           in not (T.null cleanBase) ==>
                let mUntagged = ModelName cleanBase
                    mTagged = ModelName (cleanBase <> ":latest")
                    foundUntagged = rmName <$> findRunningModel mUntagged models
                    foundTagged = rmName <$> findRunningModel mTagged models
                 in foundUntagged === foundTagged
    ]
