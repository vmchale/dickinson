module Language.Dickinson.Error ( DickinsonError (..)
                                ) where

import           Language.Dickinson.Type

-- type error but I'll do that later
data DickinsonError name a = UnfoundName a (name a)
                           | TypeMismatch a (Expression name a) DickinsonTy DickinsonTy
                           | NoMain -- separate from UnfoundName since there is no loc
                           | MultipleNames a (name a) -- top-level identifier defined more than once
