module Language.Dickinson.Type ( Dickinson
                               , Declaration (..)
                               , Expression (..)
                               ) where

import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.Text               as T
import           Language.Dickinson.Name

-- lol functorial modules
-- also fuck I need English (?) grammatical features -> at the very least,
-- "agreement"
--
-- probability-wise, might be good to look at Hakaru, monadic probability
-- especially Hakaru (basically monadic text for types idk)
--
-- interpolation might be nice
--
-- -> might want a real type system

type Dickinson name a = [Declaration name a]

data Declaration name a = Define a (name a) (Expression name a)

data Expression name a = Literal a !T.Text
                       | Choice a !(NonEmpty (Double, Expression name a))
                       | Let a ![(name a, Expression name a)] !(Expression name a)
