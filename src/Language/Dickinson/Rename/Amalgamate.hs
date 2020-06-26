{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Rename.Amalgamate ( amalgamateM
                                            ) where

import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.State        (MonadState)
import           Language.Dickinson.Error
import           Language.Dickinson.Lexer
import           Language.Dickinson.Lib.Get
import           Language.Dickinson.Type

-- sequence?
amalgamateM :: (HasLexerState s, MonadIO m, MonadError (DickinsonError AlexPosn) m, MonadState s m)
            => [FilePath] -- ^ Includes
            -> Dickinson AlexPosn
            -> m [Declaration AlexPosn]
amalgamateM _ (Dickinson [] ds) = pure ds
