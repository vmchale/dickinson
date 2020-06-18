{-# LANGUAGE OverloadedStrings #-}

module Eval ( evalTests
            ) where

import           Language.Dickinson
import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (Assertion, testCase, (@?=))

evalTests :: TestTree
evalTests = testGroup "Evalutation test"
    [ testCase "Should evalutate to a constant" constEval
    , testCase "Should allow declarations in other orders" scopeEval
    , testCase "Should allow higher-order functions" higherOrderEval
    ]

constEval :: Assertion
constEval = do
    res <- evalFile "test/eval/context.dck"
    res @?= "woman"

scopeEval :: Assertion
scopeEval = do
    res <- evalFile "test/demo/circular.dck"
    res @?= "a"

higherOrderEval :: Assertion
higherOrderEval = do
    res <- evalFile "test/data/higherOrder.dck"
    res @?= "It's me"
