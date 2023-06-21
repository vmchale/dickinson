{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Lib.Get ( parseImportM
                                  , parseFpM
                                  , parseBSLM
                                  ) where

import           Control.Composition       ((.*))
import           Control.Monad.Except      (MonadError, throwError)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.State       (MonadState)
import           Data.Functor              (($>))
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Language.Dickinson.Error
import           Language.Dickinson.Import
import           Language.Dickinson.Lexer
import           Language.Dickinson.Parser
import           Language.Dickinson.Type
import           Lens.Micro.Mtl            (use, (.=))

parseImportM :: (HasLexerState s, MonadState s m, MonadError (DickinsonError AlexPosn) m, MonadIO m)
             => [FilePath] -- ^ Include path
             -> Import AlexPosn
             -> m (Dickinson AlexPosn)
parseImportM = liftLexerState .* parseImport

liftLexerState :: (HasLexerState s, MonadState s m)
               => (AlexUserState -> m (AlexUserState, a))
               -> m a
liftLexerState fAct = do
    lSt <- use lexerStateLens
    (st, x) <- fAct lSt
    (lexerStateLens .= st) $> x

-- | Parse an import. Does not perform renaming!
parseImport :: (MonadError (DickinsonError AlexPosn) m, MonadIO m)
            => [FilePath] -- ^ Include path
            -> Import AlexPosn
            -> AlexUserState -- ^ Lexer state
            -> m (AlexUserState, Dickinson AlexPosn)
parseImport is (Import l n) lSt = do
    preFp <- resolveImport is n
    case preFp of
        Just fp -> parseFp fp lSt
        Nothing -> throwError $ ModuleNotFound l n

parseBSL :: (MonadError (DickinsonError AlexPosn) m)
         => FilePath
         -> T.Text
         -> AlexUserState
         -> m (AlexUserState, Dickinson AlexPosn)
parseBSL fp bsl lSt =
    case parseWithCtx bsl lSt of
        Right x  -> pure x
        Left err -> throwError (ParseErr fp err)

parseFp :: (MonadError (DickinsonError AlexPosn) m, MonadIO m)
        => FilePath -- ^ Source file
        -> AlexUserState -- ^ Lexer state
        -> m (AlexUserState, Dickinson AlexPosn)
parseFp fp lSt = do
    bsl <- liftIO $ TIO.readFile fp
    parseBSL fp bsl lSt

parseFpM :: (HasLexerState s, MonadState s m, MonadError (DickinsonError AlexPosn) m, MonadIO m)
        => FilePath -- ^ Source file
        -> m (Dickinson AlexPosn)
parseFpM fp = liftLexerState (parseFp fp)

parseBSLM :: (HasLexerState s, MonadState s m, MonadError (DickinsonError AlexPosn) m)
        => FilePath -- ^ Source file name (for error reporting)
        -> T.Text
        -> m (Dickinson AlexPosn)
parseBSLM fp bsl = liftLexerState (parseBSL fp bsl)
