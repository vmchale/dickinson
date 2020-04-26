module Language.Dickinson.Rename ( renameDickinson
                                 ) where

import           Language.Dickinson.Name
import           Language.Dickinson.Type

renameDickinson :: Dickinson Name a -> Dickinson Name a
renameDickinson = id
