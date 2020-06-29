{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.File ( evalFile
                               , checkFile
                               , warnFile
                               , tcFile
                               , amalgamateRename
                               , amalgamateRenameM
                               , fmtFile
                               , pipeline
                               ) where

import           Control.Applicative                   ((<|>))
import           Control.Exception                     (Exception)
import           Control.Exception.Value
import           Control.Monad                         ((<=<))
import           Control.Monad.Except                  (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class                (MonadIO)
import           Control.Monad.State                   (MonadState, StateT, evalStateT)
import qualified Data.ByteString.Lazy                  as BSL
import           Data.Semigroup                        ((<>))
import           Data.Text                             as T
import           Data.Text.Prettyprint.Doc             (hardline, pretty)
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import           Language.Dickinson.Check
import           Language.Dickinson.DuplicateCheck
import           Language.Dickinson.Error
import           Language.Dickinson.Eval
import           Language.Dickinson.Lexer
import           Language.Dickinson.Parser
import           Language.Dickinson.Rename
import           Language.Dickinson.Rename.Amalgamate
import           Language.Dickinson.ScopeCheck
import           Language.Dickinson.Type
import           Language.Dickinson.TypeCheck
import           System.Random                         (StdGen, newStdGen, randoms)

data AmalgamateSt = AmalgamateSt { amalgamateRenames    :: Renames
                                 , amalgamateLexerState :: AlexUserState
                                 }

type AllM = StateT (EvalSt AlexPosn) (ExceptT (DickinsonError AlexPosn) IO)

evalIO :: AllM x -> IO (Either (DickinsonError AlexPosn) x)
evalIO me = (\g -> evalAllWithGen g me) =<< newStdGen

evalAllWithGen :: StdGen
               -> AllM x
               -> IO (Either (DickinsonError AlexPosn) x)
evalAllWithGen g me = runExceptT $ evalStateT me (EvalSt (randoms g) mempty initRenames mempty alexInitUserState emptyTyEnv)

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

-- TODO: smart formatter
fmtFile :: FilePath -> IO ()
fmtFile = putDoc . (<> hardline) . pretty . eitherThrow . parse <=< BSL.readFile

-- | Check scoping
checkFile :: [FilePath] -> FilePath -> IO ()
checkFile = ioChecker checkScope

-- | Run some lints
warnFile :: FilePath -> IO ()
warnFile = maybeThrowIO . (\x -> checkDuplicates x <|> checkMultiple x) . (\(Dickinson _ d) -> d)
    <=< eitherThrowIO . parse
    <=< BSL.readFile

ioChecker :: Exception e => ([Declaration AlexPosn] -> Maybe e) -> [FilePath] -> FilePath -> IO ()
ioChecker checker is = maybeThrowIO . checker <=< amalgamateRename is

tcFile :: [FilePath] -> FilePath -> IO ()
tcFile is = eitherThrowIO . tyRun <=< amalgamateRename is

evalFile :: [FilePath] -> FilePath -> IO T.Text
evalFile is = fmap eitherThrow . evalIO . (evalDickinsonAsMain <=< amalgamateRenameM is)

pipeline :: [FilePath] -> FilePath -> IO T.Text
pipeline is fp = fmap eitherThrow $ evalIO $ do
    ds <- amalgamateRenameM is fp
    maybeThrow $ checkScope ds
    evalDickinsonAsMain ds
