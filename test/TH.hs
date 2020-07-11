{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TH ( thTests
          ) where

import           Language.Dickinson
import           Language.Dickinson.TH
import           Test.Tasty
import           Test.Tasty.HUnit

thDecl :: [Declaration AlexPosn]
thDecl = $(dickinson [] "test/eval/context.dck")

thTests :: TestTree
thTests =
    testCase "Returns result" $ do
        res <- run thDecl
        res @?= "woman"
