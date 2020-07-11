{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module QQ ( qqTests
          ) where

import           Language.Dickinson
import           Language.Dickinson.QuasiQuoter
import           Test.Tasty
import           Test.Tasty.HUnit

qqDecl :: [Declaration AlexPosn]
qqDecl = $(dickinson [] "test/eval/context.dck")

qqTests :: TestTree
qqTests =
    testCase "Returns result" $ do
        res <- run qqDecl
        res @?= "woman"
