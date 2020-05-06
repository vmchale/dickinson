module Language.Dickinson.Type ( Dickinson
                               , Declaration (..)
                               , Expression (..)
                               ) where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Text          as T

type Dickinson name a = [Declaration name a]

data Declaration name a = Define a (name a) (Expression name a)

data Expression name a = Literal a !T.Text
                       | Choice a !(NonEmpty (Double, Expression name a))
                       | Let a !(NonEmpty (name a, Expression name a)) !(Expression name a)
                       | Var a (name a)
                       | Concat a !(NonEmpty (Expression name a))
                       -- TODO: normalize subtree
