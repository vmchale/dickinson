{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception.Value            (eitherThrow)
import           Data.Either                        (isRight)
import           Data.List.NonEmpty                 (NonEmpty (..))
import           Data.Maybe                         (isJust)
import qualified Data.Text.IO                       as TIO
import           Eval
import           Golden
import           Language.Dickinson.Check.Duplicate
import           Language.Dickinson.Check.Internal
import           Language.Dickinson.File
import           Language.Dickinson.Import
import           Language.Dickinson.Lexer
import           Language.Dickinson.Name
import           Language.Dickinson.Parser
import           Language.Dickinson.Type
import           Language.Dickinson.Unique
import           Test.Tasty
import           Test.Tasty.HUnit
import           TH
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
        [ parseNoError "test/data/const.dck"
        , parseNoError "test/data/let.dck"
        , parseNoError "test/data/nestLet.dck"
        , parseNoError "lib/color.dck"
        , parseNoError "lib/birds.dck"
        , parseNoError "test/data/import.dck"
        , parseNoError "test/data/multiStr.dck"
        , parseNoError "examples/shakespeare.dck"
        , detectBadBranch "test/demo/sillyOption.dck"
        , parseNoError "test/data/multiQuoteify.dck"
        , findPath
        , sanityCheckTest "test/data/adt.dck"
        , thTests
        ]

findPath :: TestTree
findPath = testCase "Finds import at correct path" $ do
    res <- resolveImport ["lib", "."] (Name ("color" :| []) dummyUnique undefined)
    res @?= Just "lib/color.dck"

readNoFail :: FilePath -> IO (Dickinson AlexPosn)
readNoFail = fmap (eitherThrow . parse) . TIO.readFile

detectBadBranch :: FilePath -> TestTree
detectBadBranch fp = testCase "Detects suspicious branch" $ do
    (Dickinson _ parsed) <- readNoFail fp
    assertBool fp $ isJust (checkDuplicates parsed)

parseNoError :: FilePath -> TestTree
parseNoError fp = testCase ("Parsing doesn't fail (" ++ fp ++ ")") $ do
    contents <- TIO.readFile fp
    assertBool "Doesn't fail parsing" $ isRight (parse contents)

-- sanity check the renamer
sanityCheckTest :: FilePath -> TestTree
sanityCheckTest fp = testCase fp $
    fmap eitherThrow $ evalIO $ do
        ds <- amalgamateRenameM [] fp
        sanityCheck ds
