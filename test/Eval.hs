{-# LANGUAGE OverloadedStrings #-}

module Eval ( evalTests
            ) where

import Language.Dickinson
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, (@?=), testCase)

evalTests :: TestTree
evalTests = testCase "Should evalutate to a constant" constEval

constEval :: Assertion
constEval = do
    res <- evalFile "test/eval/context.dck"
    res @?= "woman"
