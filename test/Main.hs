module Main (main) where

import Ollama
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ 
        testForGenerate,
        testForChat,
        testForEmbedd,
        testForCopy,
        testForDelete,
        testForCreate,
        testForPs,
        testForPull,
        testForPush,
        testForShow,
        testForList
    ]

testForGenerate :: TestTree
testForGenerate = testGroup "Generate" [
    testGenerate,
    testGenerateOps,
    testGenerateOpsReturningResponse,
    testGenerateReturningResponse
  ]

testForChat :: TestTree
testForChat = testGroup "Chat" [
    testChat,
    testChatOps,
    testChatReturning,
    testChatOpsReturning
  ]

testForEmbedd :: TestTree
testForEmbedd = testGroup "Embedd" [
    testEmbedd,
    testEmbeddOps
  ]

testForCopy :: TestTree
testForCopy = testCase "copy model" $ 
    copyModel "phi3" "myPhi3" =?= ()