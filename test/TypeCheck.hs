module TypeCheck ( tcTests
                 ) where

import           Language.Dickinson.File
import           Test.Tasty
import           Test.Tasty.HUnit

tcTests :: TestTree
tcTests = testGroup "Typecheck test"
    [ testCase "Works on :match" testMatchTc
    , testCase "Currying" testCurry
    , testCase "See ADTs" testAdtTc
    , testCase "Currying (prelude functions)" testCurryPrelude
    ]

testCurry :: Assertion
testCurry = tcFile [] "test/data/quoteify.dck"

testMatchTc :: Assertion
testMatchTc = tcFile [] "test/eval/match.dck"

testAdtTc :: Assertion
testAdtTc = tcFile [] "test/data/adt.dck"

testCurryPrelude :: Assertion
testCurryPrelude = tcFile [] "prelude/curry.dck"
