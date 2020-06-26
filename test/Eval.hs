{-# LANGUAGE OverloadedStrings #-}

module Eval ( evalTests
            ) where

import           Language.Dickinson.File
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (Assertion, testCase, (@?=))

evalTests :: TestTree
evalTests = testGroup "Evalutation test"
    [ testCase "Should evalutate to a constant" constEval
    , testCase "Should allow declarations in other orders" scopeEval
    , testCase "Should allow higher-order functions" higherOrderEval
    , resultCase "test/demo/animal.dck"
    , resultCase "test/data/tuple.dck"
    , resultCase "test/demo/tyAnnot.dck"
    , resultCase "test/data/quoteify.dck"
    ]

forceText :: a -> Assertion
forceText = (`seq` pure ())

resultCase :: FilePath -> TestTree
resultCase fp = testCase fp $ result fp

result :: FilePath -> Assertion
result fp = do
    res <- evalFile ["lib"] fp
    forceText res

constEval :: Assertion
constEval = do
    res <- evalFile [] "test/eval/context.dck"
    res @?= "woman"

scopeEval :: Assertion
scopeEval = do
    res <- evalFile [] "test/demo/circular.dck"
    res @?= "a"

higherOrderEval :: Assertion
higherOrderEval = do
    res <- evalFile [] "test/data/higherOrder.dck"
    res @?= "It's me"
