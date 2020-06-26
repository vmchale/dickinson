module TypeCheck ( tcTests
                 ) where

import           Language.Dickinson.File
import           Test.Tasty
import           Test.Tasty.HUnit

tcTests :: TestTree
tcTests = testGroup "Typecheck test" $
    [ testCase "Works on :match" testMatchTc
    , testCase "Currying" testCurry
    ]

testCurry :: Assertion
testCurry = tcFile "test/data/quoteify.dck"

testMatchTc :: Assertion
testMatchTc = tcFile "test/eval/match.dck"
