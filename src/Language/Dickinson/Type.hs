module Language.Dickinson.Type ( Dickinson
                               , Declaration (..)
                               , Expression (..)
                               ) where

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

type Dickinson name a = [Declaration name a]

data Declaration name a = Define a (name a) (Expression a)

data Expression a = Literal a !T.Text
                  | Choice a !Double !T.Text
                  -- Generalize?
