{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Prettyprint.Doc.Ext ( prettyText
                                     , prettyLazyText
                                     , smartDickinson
                                     , dickinsonText
                                     , dickinsonLazyText
                                     , intercalate
                                     , hardSep
                                     , prettyDumpBinds
                                     -- * Debug class
                                     , Debug (..)
                                     -- * Operators
                                     , (<#>)
                                     , (<:>)
                                     , (<^>)
                                     , (<#*>)
                                     ) where

import qualified Data.IntMap                           as IM
import           Data.List                             (intersperse)
import           Data.Semigroup                        ((<>))
import qualified Data.Text                             as T
import qualified Data.Text.Lazy                        as TL
import           Data.Text.Prettyprint.Doc             (Doc, LayoutOptions (LayoutOptions),
                                                        PageWidth (AvailablePerLine), Pretty (pretty), SimpleDocStream,
                                                        concatWith, flatAlt, hardline, indent, layoutSmart, softline,
                                                        vsep, (<+>))
import           Data.Text.Prettyprint.Doc.Render.Text (renderLazy, renderStrict)

infixr 6 <#>
infixr 6 <:>
infixr 6 <^>

class Debug a where
    debug :: a -> Doc b

(<#>) :: Doc a -> Doc a -> Doc a
(<#>) x y = x <> hardline <> y

(<:>) :: Doc a -> Doc a -> Doc a
(<:>) x y = x <> softline <> y

(<#*>) :: Doc a -> Doc a -> Doc a
(<#*>) x y = x <> hardline <> indent 2 y

(<^>) :: Doc a -> Doc a -> Doc a
(<^>) x y = flatAlt (x <> hardline <> indent 2 y) (x <+> y)

prettyDumpBinds :: Pretty b => IM.IntMap b -> Doc a
prettyDumpBinds b = vsep (prettyBind <$> IM.toList b)

prettyBind :: Pretty b => (Int, b) -> Doc a
prettyBind (i, j) = pretty i <+> "→" <+> pretty j

hardSep :: [Doc ann] -> Doc ann
hardSep = concatWith (<#>)

intercalate :: Doc a -> [Doc a] -> Doc a
intercalate x = mconcat . intersperse x

dickinsonLayoutOptions :: LayoutOptions
dickinsonLayoutOptions = LayoutOptions (AvailablePerLine 160 0.8)

-- TODO: use layoutCompact for errors?

smartDickinson :: Doc a -> SimpleDocStream a
smartDickinson = layoutSmart dickinsonLayoutOptions

dickinsonText :: Doc a -> T.Text
dickinsonText = renderStrict . smartDickinson

dickinsonLazyText :: Doc a -> TL.Text
dickinsonLazyText = renderLazy . smartDickinson

prettyText :: Pretty a => a -> T.Text
prettyText = dickinsonText . pretty

prettyLazyText :: Pretty a => a -> TL.Text
prettyLazyText = dickinsonLazyText . pretty
