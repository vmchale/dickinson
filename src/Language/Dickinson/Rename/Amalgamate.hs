{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Rename.Amalgamate ( amalgamateM
                                            ) where

import Language.Dickinson.Type
import Control.Monad.IO.Class (MonadIO)
import Language.Dickinson.Error
import Control.Monad.Except (MonadError)

-- difference list? sequence?
amalgamateM :: (MonadIO m, MonadError (DickinsonError a) m)
            => [FilePath] -- ^ Includes
            -> Dickinson a
            -> m [Declaration a]
amalgamateM is (Dickinson _ ds) = pure ds
