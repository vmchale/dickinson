{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Rename.Amalgamate ( amalgamateM
                                            ) where

import           Control.Monad.Except     (MonadError)
import           Control.Monad.IO.Class   (MonadIO)
import           Language.Dickinson.Error
import           Language.Dickinson.Type

-- sequence?
amalgamateM :: (MonadIO m, MonadError (DickinsonError a) m)
            => [FilePath] -- ^ Includes
            -> Dickinson a
            -> m [Declaration a]
amalgamateM is (Dickinson _ ds) = pure ds
