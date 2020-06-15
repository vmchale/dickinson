module Main (main) where

import           Control.Exception    (throw)
import qualified Data.ByteString.Lazy as BSL
import           Data.Either          (isRight)
import           Data.Maybe           (isJust)
import           Golden
import           Language.Dickinson
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main =
    defaultMain $
        testGroup "All tests"
            [ goldenTests
            , parserTests
            ]

parserTests :: TestTree
parserTests =
    testGroup "Parser tests"
        [ lexNoError "test/data/let.dck"
        , parseNoError "test/data/const.dck"
        , parseNoError "test/data/let.dck"
        , parseNoError "test/data/nestLet.dck"
        , parseNoError "lib/color.dck"
        , parseNoError "lib/birds.dck"
        , lexNoError "test/data/import.dck"
        , parseNoError "test/data/import.dck"
        , detectDuplicate "test/data/multiple.dck"
        , lexNoError "examples/shakespeare.dck"
        , parseNoError "examples/shakespeare.dck"
        ]

detectDuplicate :: FilePath -> TestTree
detectDuplicate fp = testCase ("Detects duplicate name (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    let parsed = either throw id $ parse contents
    assertBool "Detects duplicate" $ isJust (checkMultiple parsed)

-- golden tests?

parseNoError :: FilePath -> TestTree
parseNoError fp = testCase ("Parsing doesn't fail (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    assertBool "Doesn't fail parsing" $ isRight (parse contents)

lexNoError :: FilePath -> TestTree
lexNoError fp = testCase ("Lexing doesn't fail (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    assertBool "Doesn't fail lexing" $ isRight (lexDickinson contents)
