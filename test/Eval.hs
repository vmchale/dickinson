{-# LANGUAGE OverloadedStrings #-}

module Eval ( evalTests
            ) where

import qualified Data.Text               as T
import           Language.Dickinson.File
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (Assertion, testCase, (@?=))

evalTests :: TestTree
evalTests = testGroup "Evaluation test"
    [ testCase "Should evalutate to a constant" constEval
    , testCase "Should allow declarations in other orders" scopeEval
    , testCase "Should allow higher-order functions" higherOrderEval
    , resultCase "test/demo/animal.dck"
    , resultCase "test/data/tuple.dck"
    , resultCase "test/demo/tyAnnot.dck"
    , resultCase "test/data/quoteify.dck"
    , resultCase "test/data/multiQuoteify.dck"
    , resultCase "test/data/hangIndefinitely.dck"
    , resolveCase "test/data/hangIndefinitely.dck"
    , testCase "Should handle interpolated multiline strings" multiQuoteEval
    , testCase "Should handle nested interpolations" multiInterpolatedNestedEval
    ]

forceResult :: a -> Assertion
forceResult = (`seq` pure ())

resultCase :: FilePath -> TestTree
resultCase fp = testCase fp $ result fp

resolveCase :: FilePath -> TestTree
resolveCase fp = testCase fp $ resolve fp

resolve :: FilePath -> Assertion
resolve fp = do
    res <- resolveFile ["prelude", "lib"] fp
    forceResult res

result :: FilePath -> Assertion
result fp = do
    res <- evalFile ["prelude", "lib"] fp
    forceResult res

evalTo :: FilePath -> T.Text -> Assertion
evalTo fp t = do
    res <- evalFile [] fp
    res @?= t

constEval :: Assertion
constEval = evalTo "test/eval/context.dck" "woman"

scopeEval :: Assertion
scopeEval = evalTo "test/demo/circular.dck" "a"

higherOrderEval :: Assertion
higherOrderEval = evalTo "test/data/higherOrder.dck" "It's me"

multiQuoteEval :: Assertion
multiQuoteEval = evalTo "test/data/multiQuoteify.dck" "God created war so that Americans would learn geography.\n    — Mark Twain"

multiInterpolatedNestedEval :: Assertion
multiInterpolatedNestedEval = evalTo "test/data/interpolateNested.dck" "This is an interpolated string sort of."
