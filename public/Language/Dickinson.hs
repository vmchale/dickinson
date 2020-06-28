-- | This modules contains some bits and pieces to work with Dickinson code.
module Language.Dickinson ( -- * Parser
                            parse
                          , ParseError (..)
                          -- * Lexer
                          , lexDickinson
                          , AlexPosn
                          , Token (..)
                          -- * AST
                          , Dickinson
                          , Declaration (..)
                          , Expression (..)
                          , Pattern (..)
                          , DickinsonTy (..)
                          , Name
                          -- * Imports
                          , resolveImport
                          -- * Version info
                          , dickinsonVersion
                          , dickinsonVersionString
                          ) where

import qualified Data.Version              as V
import           Language.Dickinson.Import
import           Language.Dickinson.Lexer
import           Language.Dickinson.Name
import           Language.Dickinson.Parser
import           Language.Dickinson.Type
import qualified Paths_language_dickinson  as P

dickinsonVersion :: V.Version
dickinsonVersion = P.version

dickinsonVersionString :: String
dickinsonVersionString = V.showVersion dickinsonVersion
