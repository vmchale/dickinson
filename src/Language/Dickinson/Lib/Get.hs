{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Lib.Get ( parseImport
                                  , parseImportM
                                  ) where

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
parseImportM is i = do
    lSt <- use lexerStateLens
    (st, d) <- parseImport is i lSt
    (lexerStateLens .= st) $> d

-- Parse an import. Does NOT perform renaming!
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
