module Language.Dickinson ( parse
                          , lexDickinson
                          , prettyDickinson
                          -- * Types
                          , Dickinson
                          , Declaration (..)
                          -- * Reëxports from
                          -- "Data.Text.Prettyprint.Doc.Render.Text"
                          , renderLazy
                          , renderStrict
                          , Pretty (pretty)
                          , putDoc
                          -- * Helpers
                          , prettyLazyText
                          ) where

import           Data.Text.Prettyprint.Doc             (Pretty (pretty))
import           Data.Text.Prettyprint.Doc.Ext
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc, renderLazy,
                                                        renderStrict)
import           Language.Dickinson.Lexer
import           Language.Dickinson.Parser
import           Language.Dickinson.Pretty
import           Language.Dickinson.Type
