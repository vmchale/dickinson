{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Lib.Get ( parseImport
                                  ) where

import           Control.Monad.Except      (MonadError, throwError)
import           Control.Monad.IO.Class    (MonadIO (..))
import qualified Data.ByteString.Lazy      as BSL
import           Language.Dickinson.Error
import           Language.Dickinson.Import
import           Language.Dickinson.Lexer
import           Language.Dickinson.Parser
import           Language.Dickinson.Type

-- Parse an import. Does NOT perform renaming!
parseImport :: (MonadError (DickinsonError AlexPosn) m, MonadIO m)
            => [FilePath] -- ^ Include path
            -> Import AlexPosn
            -> AlexUserState -- ^ Lexer state
            -> m (AlexUserState, Dickinson AlexPosn)
parseImport is (Import l n) lSt = do
    preFp <- resolveImport is n
    case preFp of
        Just fp -> do
            bsl <- liftIO $ BSL.readFile fp
            case parseWithCtx bsl lSt of
                Right x  -> pure x
                Left err -> throwError (ParseErr fp err)
        Nothing -> throwError $ ModuleNotFound l n
