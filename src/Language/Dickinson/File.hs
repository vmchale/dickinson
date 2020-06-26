{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.File ( evalFile
                               , checkFile
                               , warnFile
                               , tcFile
                               , amalgamateRenameM
                               -- , fmtFile
                               ) where

import           Control.Applicative                   ((<|>))
import           Control.Exception                     (Exception, throw, throwIO)
import           Control.Monad                         ((<=<))
import           Control.Monad.Except                  (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class                (MonadIO)
import           Control.Monad.State                   (MonadState, StateT, evalStateT)
import           Data.Semigroup                        ((<>))
import           Data.Text                             as T
import           Data.Text.Prettyprint.Doc             (hardline, pretty)
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import           Language.Dickinson.Check
import           Language.Dickinson.DuplicateCheck
import           Language.Dickinson.Error
import           Language.Dickinson.Eval
import           Language.Dickinson.Lexer
import           Language.Dickinson.Rename
import           Language.Dickinson.Rename.Amalgamate
import           Language.Dickinson.ScopeCheck
import           Language.Dickinson.Type
import           Language.Dickinson.TypeCheck

data AmalgamateSt = AmalgamateSt { amalgamateRenames    :: Renames
                                 , amalgamateLexerState :: AlexUserState
                                 }

type AmalgamateM = ExceptT (DickinsonError AlexPosn) (StateT AmalgamateSt IO)

initAmalgamateSt :: AmalgamateSt
initAmalgamateSt = AmalgamateSt (initRenames 0) alexInitUserState

instance HasLexerState AmalgamateSt where
    lexerStateLens f s = fmap (\x -> s { amalgamateLexerState = x }) (f (amalgamateLexerState s))

instance HasRenames AmalgamateSt where
    rename f s = fmap (\x -> s { amalgamateRenames = x }) (f (amalgamateRenames s))

amalgamateRenameM :: (HasRenames s, HasLexerState s, MonadIO m, MonadError (DickinsonError AlexPosn) m, MonadState s m)
                  => [FilePath]
                  -> FilePath
                  -> m [Declaration AlexPosn]
amalgamateRenameM is = (balanceMax *>) . renameDickinsonM <=< fileDecls is

amalgamateRename :: [FilePath]
                 -> FilePath
                 -> IO [Declaration AlexPosn]
amalgamateRename is fp = flip evalStateT initAmalgamateSt $ fmap yeet $ runExceptT $ amalgamateRenameM is fp

-- fmtFile :: FilePath -> IO ()
-- fmtFile = putDoc . (<> hardline) . pretty . go <=< BSL.readFile
    -- where go = fst . uncurry renameDickinson . yeet . parseWithMax

-- | Check scoping
checkFile :: FilePath -> IO ()
checkFile = ioChecker checkScope

-- | Run some lints
warnFile :: FilePath -> IO ()
warnFile = ioChecker (\x -> checkDuplicates x <|> checkMultiple x)

ioChecker :: Exception e => ([Declaration AlexPosn] -> (Maybe e)) -> FilePath -> IO ()
ioChecker checker = go . checker <=< amalgamateRename []
    where go (Just err) = throwIO err
          go Nothing    = pure ()

tcFile :: FilePath -> IO () -- TODO: includes
tcFile = yeetIO . tyRun <=< amalgamateRename []

-- TODO: runDeclarationM
evalFile :: [FilePath] -> FilePath -> IO T.Text
evalFile is = fmap yeet . evalIO alexInitUserState . (evalDickinsonAsMain <=< amalgamateRenameM is)

yeetIO :: Exception e => Either e x -> IO x
yeetIO = either throwIO pure

yeet :: Exception e => Either e x -> x
yeet = either throw id
