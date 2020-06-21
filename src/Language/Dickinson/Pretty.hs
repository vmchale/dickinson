module Language.Dickinson.Pretty ( prettyDickinson
                                 ) where

import           Data.Semigroup            ((<>))
import           Data.Text.Prettyprint.Doc (Doc, Pretty (pretty), concatWith, hardline)
import           Language.Dickinson.Type

prettyDickinson :: Dickinson a -> Doc b
prettyDickinson = concatWith (\x y -> x <> hardline <> hardline <> y) . fmap pretty
