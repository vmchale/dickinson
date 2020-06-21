module TypeCheck ( tcTests
                 ) where

import           Language.Dickinson.File
import           Test.Tasty
import           Test.Tasty.HUnit

tcTests :: TestTree
tcTests = testCase "Works on :match" $ testMatchTc

testMatchTc :: Assertion
testMatchTc = tcFile "examples/doggo.dck"
