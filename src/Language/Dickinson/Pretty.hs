module Language.Dickinson.Pretty ( prettyDickinson
                                 ) where

import           Data.Text.Prettyprint.Doc     (Doc, Pretty (pretty), concatWith)
import           Data.Text.Prettyprint.Doc.Ext ((<#>))
import           Language.Dickinson.Type

prettyDickinson :: Dickinson a -> Doc b
prettyDickinson = concatWith (<#>) . fmap pretty
