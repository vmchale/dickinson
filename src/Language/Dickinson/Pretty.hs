module Language.Dickinson.Pretty ( prettyDickinson
                                 ) where

import           Data.List                 (intersperse)
import           Data.Text.Prettyprint.Doc (Doc, Pretty (pretty), hardline)
import           Language.Dickinson.Type

prettyDickinson :: Pretty (name a) => Dickinson name a -> Doc b
prettyDickinson = mconcat . intersperse hardline . fmap pretty
