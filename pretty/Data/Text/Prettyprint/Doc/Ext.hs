module Data.Text.Prettyprint.Doc.Ext ( prettyText
                                     , prettyLazyText
                                     ) where

-- TODO: internal library
import qualified Data.Text                             as T
import qualified Data.Text.Lazy                        as TL
import           Data.Text.Prettyprint.Doc             (Pretty (pretty),
                                                        defaultLayoutOptions,
                                                        layoutSmart)
import           Data.Text.Prettyprint.Doc.Render.Text (renderLazy,
                                                        renderStrict)

prettyText :: Pretty a => a -> T.Text
prettyText = renderStrict . layoutSmart defaultLayoutOptions . pretty

prettyLazyText :: Pretty a => a -> TL.Text
prettyLazyText = renderLazy . layoutSmart defaultLayoutOptions . pretty
