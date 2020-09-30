{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.File ( evalIO
                               , evalFile
                               , checkFile
                               , validateFile
                               , validateAmalgamate
                               , warnFile
                               , patternExhaustivenessFile
                               , tcFile
                               , amalgamateRename
                               , amalgamateRenameM
                               , pipeline
                               , resolveFile
                               ) where

import           Control.Applicative                  ((<|>))
import           Control.Composition                  ((.*))
import           Control.Exception                    (Exception)
import           Control.Exception.Value
import           Control.Monad                        (void, (<=<))
import           Control.Monad.Except                 (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class               (MonadIO)
import           Control.Monad.State                  (MonadState, StateT, evalStateT)
import qualified Data.ByteString.Lazy                 as BSL
import           Data.Functor                         (($>))
import           Data.Text                            as T
import           Language.Dickinson.Check
import           Language.Dickinson.Check.Duplicate
import           Language.Dickinson.Check.Exhaustive
import           Language.Dickinson.Check.Scope
import           Language.Dickinson.Error
import           Language.Dickinson.Eval
import           Language.Dickinson.Lexer
import           Language.Dickinson.Parser
import           Language.Dickinson.Pipeline
import           Language.Dickinson.Rename
import           Language.Dickinson.Rename.Amalgamate
import           Language.Dickinson.Type
import           Language.Dickinson.TypeCheck
import           System.Random                        (StdGen, newStdGen, randoms)

data AmalgamateSt = AmalgamateSt { amalgamateRenames    :: Renames
                                 , amalgamateLexerState :: AlexUserState
                                 }

type AllM = StateT (EvalSt AlexPosn) (ExceptT (DickinsonError AlexPosn) IO)

evalIO :: AllM x -> IO (Either (DickinsonError AlexPosn) x)
evalIO me = (\g -> evalAllWithGen g me) =<< newStdGen

evalAllWithGen :: StdGen
               -> AllM x
               -> IO (Either (DickinsonError AlexPosn) x)
evalAllWithGen g me = runExceptT $ evalStateT me (EvalSt (randoms g) mempty initRenames mempty alexInitUserState emptyTyEnv mempty)

initAmalgamateSt :: AmalgamateSt
initAmalgamateSt = AmalgamateSt initRenames alexInitUserState

instance HasLexerState AmalgamateSt where
    lexerStateLens f s = fmap (\x -> s { amalgamateLexerState = x }) (f (amalgamateLexerState s))

instance HasRenames AmalgamateSt where
    rename f s = fmap (\x -> s { amalgamateRenames = x }) (f (amalgamateRenames s))

amalgamateRenameM :: (HasRenames s, HasLexerState s, MonadIO m, MonadError (DickinsonError AlexPosn) m, MonadState s m)
                  => [FilePath]
                  -> FilePath
                  -> m [Declaration AlexPosn]
amalgamateRenameM is = (balanceMax *>) . renameDeclarationsM <=< fileDecls is

amalgamateRename :: [FilePath]
                 -> FilePath
                 -> IO [Declaration AlexPosn]
amalgamateRename is fp = flip evalStateT initAmalgamateSt $ fmap eitherThrow $ runExceptT $ amalgamateRenameM is fp

-- | Check scoping
checkFile :: [FilePath] -> FilePath -> IO ()
checkFile = ioChecker checkScope

-- | Check scoping and types
validateFile :: [FilePath] -> FilePath -> IO ()
validateFile = void .* validateAmalgamate

validateAmalgamate :: [FilePath] -> FilePath -> IO [Declaration AlexPosn]
validateAmalgamate is fp = do
    d <- amalgamateRename is fp
    maybeThrowIO $ checkScope d
    eitherThrowIO (tyRun d) $> d

-- | Run some lints
warnFile :: FilePath -> IO ()
warnFile = maybeThrowIO . (\x -> checkDuplicates x <|> checkMultiple x) . modDefs
    <=< eitherThrowIO . parse
    <=< BSL.readFile

ioChecker :: Exception e => ([Declaration AlexPosn] -> Maybe e) -> [FilePath] -> FilePath -> IO ()
ioChecker checker is = maybeThrowIO . checker <=< amalgamateRename is

tcFile :: [FilePath] -> FilePath -> IO ()
tcFile is = eitherThrowIO . tyRun <=< amalgamateRename is

patternExhaustivenessFile :: [FilePath] -> FilePath -> IO ()
patternExhaustivenessFile is = maybeThrowIO . checkExhaustive <=< amalgamateRename is

evalFile :: [FilePath] -> FilePath -> IO T.Text
evalFile is fp = (\g -> evalFileGen g is fp) =<< newStdGen

evalFileGen :: StdGen -> [FilePath] -> FilePath -> IO T.Text
evalFileGen g is = fmap eitherThrow . evalAllWithGen g . (evalDickinsonAsMain <=< amalgamateRenameM is)

resolveFile :: [FilePath] -> FilePath -> IO [Declaration AlexPosn]
resolveFile is = fmap eitherThrow . evalIO . (traverse resolveDeclarationM <=< amalgamateRenameM is)

pipeline :: [FilePath] -> FilePath -> IO T.Text
pipeline is fp = fmap eitherThrow $ evalIO $
    checkEvalM =<< amalgamateRenameM is fp
