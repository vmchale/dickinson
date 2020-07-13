{-# LANGUAGE OverloadedStrings #-}

module Eval ( evalTests
            ) where

import           Control.Monad           ((<=<))
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
    , testCase "test/data/flattenLambda.dck" example
    , testCase "Match on ADT constructors" matchAdtEval
    ]

forceResult :: a -> Assertion
forceResult = (`seq` pure ())

resultCase :: FilePath -> TestTree
resultCase fp = testCase fp $ result fp

resolveCase :: FilePath -> TestTree
resolveCase fp = testCase fp $ resolve fp

resolve :: FilePath -> Assertion
resolve = forceResult <=< resolveFile ["prelude", "lib"]

result :: FilePath -> Assertion
result = forceResult <=< evalFile ["prelude", "lib"]

example :: Assertion
example = forceResult =<< evalFile ["examples"] "test/data/flattenLambda.dck"

evalTo :: FilePath -> T.Text -> Assertion
evalTo fp t = do
    res <- evalFile [] fp
    res @?= t

constEval :: Assertion
constEval = evalTo "test/eval/context.dck" "woman"

scopeEval :: Assertion
scopeEval = evalTo "test/demo/circular.dck" "a"

matchAdtEval :: Assertion
matchAdtEval = evalTo "test/eval/matchSex.dck" "Maxine is a good girl. She tries her best."

higherOrderEval :: Assertion
higherOrderEval = evalTo "test/data/higherOrder.dck" "It's me"

multiQuoteEval :: Assertion
multiQuoteEval = evalTo "test/data/multiQuoteify.dck" "God created war so that Americans would learn geography.\n    — Mark Twain"

multiInterpolatedNestedEval :: Assertion
multiInterpolatedNestedEval = evalTo "test/data/interpolateNested.dck" "This is an interpolated string sort of."
