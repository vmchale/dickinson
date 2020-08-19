{-# LANGUAGE OverloadedStrings #-}

module Pattern ( patternTests
               ) where

import           Data.List.NonEmpty                 (NonEmpty (..))
import           Language.Dickinson.Name
import           Language.Dickinson.Pattern.Useless
import           Language.Dickinson.Type
import           Language.Dickinson.Unique
import           Test.Tasty
import           Test.Tasty.HUnit

patternTests :: TestTree
patternTests = testGroup "Pattern match unit tests"
    [ testSpecializeTuple
    , testSpecializeTag
    ]

-- just tests that they make sense as I understand them
testSpecializeTuple :: TestTree
testSpecializeTuple = testCase "specializeTuple" $ do
    let patternMatrix = [[Wildcard ()]]
        specializedMatrix = specializeTuple 2 patternMatrix
    specializedMatrix @?= [[Wildcard (), Wildcard ()]]

testSpecializeTag :: TestTree
testSpecializeTag = testCase "specializeTag" $ do
    let n0 = Name ("a0" :| []) (Unique 0) ()
        n1 = Name ("a1" :| []) (Unique 1) ()
        patternMatrix = [ [PatternCons () n0, Wildcard ()]
                        , [PatternCons () n1, Wildcard ()]
                        ]
        specializedMatrix = [ [Wildcard ()]
                            ]
    specializeTag n0 patternMatrix @?= specializedMatrix
