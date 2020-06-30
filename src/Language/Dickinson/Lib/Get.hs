{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Lib.Get ( parseImportM
                                  , parseFpM
                                  ) where

import           Control.Composition       ((.*))
import           Control.Monad.Except      (MonadError, throwError)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.State       (MonadState)
import qualified Data.ByteString.Lazy      as BSL
import           Data.Functor              (($>))
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

parseFp :: (MonadError (DickinsonError AlexPosn) m, MonadIO m)
        => FilePath -- ^ Source file
        -> AlexUserState -- ^ Lexer state
        -> m (AlexUserState, Dickinson AlexPosn)
parseFp fp lSt = do
    bsl <- liftIO $ BSL.readFile fp
    case parseWithCtx bsl lSt of
        Right x  -> pure x
        Left err -> throwError (ParseErr fp err)

parseFpM :: (HasLexerState s, MonadState s m, MonadError (DickinsonError AlexPosn) m, MonadIO m)
        => FilePath -- ^ Source file
        -> m (Dickinson AlexPosn)
parseFpM fp = liftLexerState (parseFp fp)
