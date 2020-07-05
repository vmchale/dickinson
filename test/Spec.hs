{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception.Value              (eitherThrow)
import qualified Data.ByteString.Lazy                 as BSL
import           Data.Either                          (isRight)
import           Data.List.NonEmpty                   (NonEmpty (..))
import           Data.Maybe                           (isJust, isNothing)
import           Eval
import           Golden
import           Language.Dickinson.Check
import           Language.Dickinson.Check.Internal
import           Language.Dickinson.DuplicateCheck
import           Language.Dickinson.File
import           Language.Dickinson.Import
import           Language.Dickinson.Lexer
import           Language.Dickinson.Name
import           Language.Dickinson.Parser
import           Language.Dickinson.Rename
import           Language.Dickinson.Rename.Amalgamate
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
        , lexNoError "test/data/lexDollarSign.dck"
        , parseNoError "test/data/import.dck"
        , detectDuplicate "test/data/multiple.dck"
        , detectDuplicate "test/error/double.dck"
        , detectDuplicate "test/error/tyDouble.dck"
        , detectDuplicate "test/error/tyConsDouble.dck"
        , lexNoError "examples/shakespeare.dck"
        , lexNoError "test/data/multiStr.dck"
        , parseNoError "test/data/multiStr.dck"
        , parseNoError "examples/shakespeare.dck"
        , detectScopeError "test/demo/improbableScope.dck"
        , detectBadBranch "test/demo/sillyOption.dck"
        , noScopeError "test/demo/circular.dck"
        , noScopeError "test/data/adt.dck"
        , detectScopeError "test/error/tyScope.dck"
        , detectScopeError "test/error/constructorScope.dck"
        , lexNoError "test/data/multiQuoteify.dck"
        , parseNoError "test/data/multiQuoteify.dck"
        , findPath
        , sanityCheckTest "test/data/adt.dck"
        ]

findPath :: TestTree
findPath = testCase "Finds import at correct path" $ do
    res <- resolveImport ["lib", "."] (Name ("color" :| []) dummyUnique undefined)
    res @?= Just "lib/color.dck"

readNoFail :: FilePath -> IO (Dickinson AlexPosn)
readNoFail = fmap (eitherThrow . parse) . BSL.readFile

detectBadBranch :: FilePath -> TestTree
detectBadBranch fp = testCase "Detects suspicious branch" $ do
    (Dickinson _ parsed) <- readNoFail fp
    assertBool fp $ isJust (checkDuplicates parsed)

detectDuplicate :: FilePath -> TestTree
detectDuplicate fp = testCase ("Detects duplicate name (" ++ fp ++ ")") $ do
    (Dickinson _ parsed) <- readNoFail fp
    assertBool fp $ isJust (checkMultiple parsed)

detectScopeError :: FilePath -> TestTree
detectScopeError fp = testCase "Finds scoping error" $ do
    (Dickinson _ renamed) <- parseRename fp
    assertBool fp $ isJust (checkScope renamed)

noScopeError :: FilePath -> TestTree
noScopeError fp = testCase "Reports valid scoping" $ do
    (Dickinson _ renamed) <- parseRename fp
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
parseRename = fmap (fst . uncurry renameDickinson . eitherThrow . parseWithMax) . BSL.readFile

-- sanity check the renamer
sanityCheckTest :: FilePath -> TestTree
sanityCheckTest fp = testCase fp $
    fmap eitherThrow $ evalIO $ do
        ds <- fileDecls [] fp
        sanityCheck ds
