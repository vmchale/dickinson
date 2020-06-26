{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception                 (throw)
import qualified Data.ByteString.Lazy              as BSL
import           Data.Either                       (isRight)
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.Maybe                        (isJust, isNothing)
import           Eval
import           Golden
import           Language.Dickinson.Check
import           Language.Dickinson.DuplicateCheck
import           Language.Dickinson.Import
import           Language.Dickinson.Lexer
import           Language.Dickinson.Name
import           Language.Dickinson.Parser
import           Language.Dickinson.Rename
import           Language.Dickinson.ScopeCheck
import           Language.Dickinson.Type
import           Language.Dickinson.Unique
import           Test.Tasty
import           Test.Tasty.HUnit
import           TypeCheck

main :: IO ()
main =
    defaultMain $
        testGroup "All tests"
            [ goldenTests
            , parserTests
            , evalTests
            , tcTests
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
        , detectScopeError "test/demo/improbableScope.dck"
        , detectBadBranch "test/demo/sillyOption.dck"
        , noScopeError "test/demo/circular.dck"
        , findPath
        ]

findPath :: TestTree
findPath = testCase "Finds import at correct path" $ do
    res <- resolveImport ["lib", "."] (Name ("color" :| []) dummyUnique undefined)
    res @?= Just "lib/color.dck"

readNoFail :: FilePath -> IO (Dickinson AlexPosn)
readNoFail = fmap (either throw id . parse) . BSL.readFile

detectBadBranch :: FilePath -> TestTree
detectBadBranch fp = testCase "Detects suspicious branch" $ do
    parsed <- readNoFail fp
    assertBool fp $ isJust (checkDuplicates parsed)

detectDuplicate :: FilePath -> TestTree
detectDuplicate fp = testCase ("Detects duplicate name (" ++ fp ++ ")") $ do
    parsed <- readNoFail fp
    assertBool fp $ isJust (checkMultiple parsed)

detectScopeError :: FilePath -> TestTree
detectScopeError fp = testCase "Finds scoping error" $ do
    renamed <- parseRename fp
    assertBool fp $ isJust (checkScope renamed)

noScopeError :: FilePath -> TestTree
noScopeError fp = testCase "Reports valid scoping" $ do
    renamed <- parseRename fp
    assertBool fp $ isNothing (checkScope renamed)

parseNoError :: FilePath -> TestTree
parseNoError fp = testCase ("Parsing doesn't fail (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    assertBool "Doesn't fail parsing" $ isRight (parse contents)

lexNoError :: FilePath -> TestTree
lexNoError fp = testCase ("Lexing doesn't fail (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    assertBool "Doesn't fail lexing" $ isRight (lexDickinson contents)

parseRename :: FilePath -> IO (Dickinson AlexPosn)
parseRename = fmap (fst . uncurry renameDickinson . either throw id . parseWithMax) . BSL.readFile
