module Pattern ( patternTests
               ) where

import           Language.Dickinson.Pattern.Useless
import           Language.Dickinson.Type
import           Test.Tasty
import           Test.Tasty.HUnit

patternTests :: TestTree
patternTests = testGroup "Pattern match unit tests"
    [ testSpecializeTuple
    ]

-- just tests that they make sense as I understand them
testSpecializeTuple :: TestTree
testSpecializeTuple = testCase "specializeTuple" $ do
    let patternMatrix = [[Wildcard ()]]
        specializedMatrix = specializeTuple 2 patternMatrix
    specializedMatrix @?= [[Wildcard (), Wildcard ()]]
