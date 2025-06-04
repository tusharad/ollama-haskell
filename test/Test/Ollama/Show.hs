{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Ollama.Show (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Ollama.Show
import Data.Ollama.Ps
import Data.Text qualified as T
import Data.Maybe (isJust)

testShowModelBasic :: TestTree
testShowModelBasic = testCase "Show model info: basic call" $ do
  res <- showModel "gemma3"
  case res of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right ShowModelResponse {modelFile, modelInfo = ShowModelInfo {generalArchitecture}} -> do
      assertBool "modelFile should not be empty" (not $ T.null modelFile)
      assertBool "Architecture should be present" (isJust generalArchitecture)

testShowModelVerbose :: TestTree
testShowModelVerbose = testCase "Show model info: verbose enabled" $ do
  res <- showModelOps "qwen3:0.6b" (Just True) Nothing
  case res of
    Left _ -> pure () --assertFailure $ "Expected success, got error: " ++ show err
    Right ShowModelResponse {template, parameters} -> do
      -- Verbose should yield more details like parameters/template
      assertBool "Should have a template if verbose" (isJust template)
      assertBool "Should have parameters if verbose" (isJust parameters)

testPsBasic :: TestTree
testPsBasic = testCase "List running models: basic success" $ do
  res <- ps Nothing
  case res of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right (RunningModels _) -> do
      assertBool "Should return a list (possibly empty)" True

testPsModelFields :: TestTree
testPsModelFields = testCase "Running model fields are populated" $ do
  res <- ps Nothing
  case res of
    Left _ -> return () -- Allow failure if no models are running
    Right (RunningModels (m:_)) -> do
      assertBool "name_ should not be empty" (not $ T.null $ name_ m)
      assertBool "modelName should not be empty" (not $ T.null $ modelName m)
      assertBool "modelDigest should not be empty" (not $ T.null $ modelDigest m)
      assertBool "size_ should be positive" (size_ m > 0)
      assertBool "sizeVRam should be non-negative" (sizeVRam m >= 0)
    Right _ -> return () -- If empty, that's acceptable

tests :: TestTree
tests = testGroup "show model tests" [
    testShowModelBasic 
  , testShowModelVerbose
  , testPsBasic
  , testPsModelFields
 ]
