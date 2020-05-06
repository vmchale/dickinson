module Main (main) where

import qualified Data.ByteString.Lazy as BSL
import           Data.Either          (isRight)
import           Language.Dickinson
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main =
    defaultMain $
        testGroup "Parser tests"
            [ parseNoError "test/data/const.dck" ]

parseNoError :: FilePath -> TestTree
parseNoError fp = testCase ("Parsing doesn't fail (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    assertBool "Doesn't fail parsing" $ isRight (parse contents)
