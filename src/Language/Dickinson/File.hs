{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.File ( evalIO
                               , evalFile
                               , checkFile
                               , validateFile
                               , validateBSL
                               , validateAmalgamate
                               , warnFile
                               , warnBSL
                               , patternExhaustivenessFile
                               , patternExhaustivenessBSL
                               , tcFile
                               , amalgamateRename
                               , amalgamateRenameM
                               , pipeline
                               , pipelineBSL
                               , pipelineBSLErr
                               , resolveFile
                               ) where

import           Control.Applicative                  ((<|>))
import           Control.Composition                  ((.*), (.**), (<=*<))
import           Control.Exception                    (Exception)
import           Control.Exception.Value
import           Control.Monad                        (void, (<=<))
import           Control.Monad.Except                 (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class               (MonadIO)
import           Control.Monad.State                  (MonadState, StateT, evalStateT)
import           Data.Bifunctor                       (first)
import qualified Data.ByteString.Lazy                 as BSL
import           Data.Functor                         (($>))
import           Data.Text                            as T
import           Data.Text.Prettyprint.Doc.Ext        (prettyText)
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
import           Prettyprinter                        (pretty)
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

amalgamateRenameInpM :: (HasRenames s, HasLexerState s, MonadIO m, MonadError (DickinsonError AlexPosn) m, MonadState s m)
                     => [FilePath]
                     -> FilePath -- ^ For error reporting
                     -> BSL.ByteString
                     -> m [Declaration AlexPosn]
amalgamateRenameInpM is = (balanceMax *>) . renameDeclarationsM <=*< bslDecls is

amalgamateRename :: [FilePath]
                 -> FilePath
                 -> IO [Declaration AlexPosn]
amalgamateRename is fp = flip evalStateT initAmalgamateSt $ fmap eitherThrow $ runExceptT $ amalgamateRenameM is fp

amalgamateRenameBSL :: [FilePath]
                    -> FilePath -- ^ For error reporting
                    -> BSL.ByteString
                    -> IO [Declaration AlexPosn]
amalgamateRenameBSL is fp bsl = flip evalStateT initAmalgamateSt $ fmap eitherThrow $ runExceptT $ amalgamateRenameInpM is fp bsl

-- | Check scoping
checkFile :: [FilePath] -> FilePath -> IO ()
checkFile = ioChecker checkScope

-- | Check scoping and types
validateFile :: [FilePath] -> FilePath -> IO ()
validateFile = void .* validateAmalgamate

-- | Check scoping and types
--
-- @since 1.4.1.0
validateBSL :: [FilePath] -> FilePath -> BSL.ByteString -> IO ()
validateBSL = void .** validateAmalgamateBSL

validateAmalgamate :: [FilePath] -> FilePath -> IO [Declaration AlexPosn]
validateAmalgamate is fp = do
    d <- amalgamateRename is fp
    maybeThrowIO $ checkScope d
    eitherThrowIO (tyRun d) $> d

validateAmalgamateBSL :: [FilePath] -> FilePath -> BSL.ByteString -> IO [Declaration AlexPosn]
validateAmalgamateBSL is fp bsl = do
    d <- amalgamateRenameBSL is fp bsl
    maybeThrowIO $ checkScope d
    eitherThrowIO (tyRun d) $> d

warnFile :: FilePath -> IO ()
warnFile = warnBSL <=< BSL.readFile

-- | Run some lints
--
-- @since 1.4.2.0
warnBSL :: BSL.ByteString -> IO ()
warnBSL = maybeThrowIO . (\x -> checkDuplicates x <|> checkMultiple x) . modDefs
    <=< eitherThrowIO . parse

ioChecker :: Exception e => ([Declaration AlexPosn] -> Maybe e) -> [FilePath] -> FilePath -> IO ()
ioChecker checker is = maybeThrowIO . checker <=< amalgamateRename is

tcFile :: [FilePath] -> FilePath -> IO ()
tcFile is = eitherThrowIO . tyRun <=< amalgamateRename is

patternExhaustivenessFile :: [FilePath] -- ^ Includes
                          -> FilePath
                          -> IO ()
patternExhaustivenessFile is = maybeThrowIO . checkExhaustive <=< amalgamateRename is

-- | @since 1.4.1.0
patternExhaustivenessBSL :: [FilePath] -- ^ Includes
                         -> FilePath -- ^ Source file (for error reporting)
                         -> BSL.ByteString
                         -> IO ()
patternExhaustivenessBSL is = maybeThrowIO . checkExhaustive <=*< amalgamateRenameBSL is

evalFile :: [FilePath] -> FilePath -> IO T.Text
evalFile is fp = (\g -> evalFileGen g is fp) =<< newStdGen

evalFileGen :: StdGen -> [FilePath] -> FilePath -> IO T.Text
evalFileGen g is = fmap eitherThrow . evalAllWithGen g . (evalDickinsonAsMain <=< amalgamateRenameM is)

resolveFile :: [FilePath] -> FilePath -> IO [Declaration AlexPosn]
resolveFile is = fmap eitherThrow . evalIO . (traverse resolveDeclarationM <=< amalgamateRenameM is)

pipeline :: [FilePath] -> FilePath -> IO T.Text
pipeline is fp = fmap eitherThrow $ evalIO $
    checkEvalM =<< amalgamateRenameM is fp

-- | @since 1.4.2.0
pipelineBSLErr :: [FilePath]
               -> FilePath -- ^ For error reporting
               -> BSL.ByteString
               -> IO (Either T.Text T.Text)
pipelineBSLErr is fp bsl = fmap (first prettyText) $ evalIO $
    checkEvalM =<< amalgamateRenameInpM is fp bsl

-- | @since 1.4.1.0
pipelineBSL :: [FilePath]
            -> FilePath -- ^ For error reporting
            -> BSL.ByteString
            -> IO T.Text
pipelineBSL is fp bsl = fmap eitherThrow $ evalIO $
    checkEvalM =<< amalgamateRenameInpM is fp bsl
