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
    , testCase "Works with :choice branches" testChoice
    ]

testChoice :: Assertion
testChoice = tcPlain "test/data/tyChoice.dck"

testCurry :: Assertion
testCurry = tcPlain "test/data/quoteify.dck"

testMatchTc :: Assertion
testMatchTc = tcPlain "test/eval/match.dck"

testAdtTc :: Assertion
testAdtTc = tcPlain "test/data/adt.dck"

testCurryPrelude :: Assertion
testCurryPrelude = tcPlain "prelude/curry.dck"

tcPlain :: FilePath -> Assertion
tcPlain = tcFile []
