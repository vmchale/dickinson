-- | This modules contains some bits and pieces to work with Dickinson code.
module Language.Dickinson ( -- * Parser
                            parse
                          , ParseError (..)
                          -- * Lexer
                          , lexDickinson
                          , AlexPosn
                          , Token
                          -- * Checks/passes
                          , checkMultiple
                          , checkDuplicates
                          , checkScope
                          , checkFile
                          , warnFile
                          -- * AST
                          , Dickinson
                          , Declaration (..)
                          , Expression (..)
                          , DickinsonTy (..)
                          , Name (..)
                          , Unique
                          -- * Errors
                          , DickinsonError (..)
                          -- * Pretty-printing
                          , prettyDickinson
                          -- * Version info
                          , dickinsonVersion
                          , dickinsonVersionString
                          ) where

import qualified Data.Version                      as V
import           Language.Dickinson.Check
import           Language.Dickinson.DuplicateCheck
import           Language.Dickinson.Error
import           Language.Dickinson.Eval
import           Language.Dickinson.Lexer
import           Language.Dickinson.Name
import           Language.Dickinson.Parser
import           Language.Dickinson.Pretty
import           Language.Dickinson.Rename
import           Language.Dickinson.ScopeCheck
import           Language.Dickinson.Type
import           Language.Dickinson.TypeCheck
import           Language.Dickinson.Unique
import           Language.Dickinson.File
import qualified Paths_language_dickinson                   as P

dickinsonVersion :: V.Version
dickinsonVersion = P.version

dickinsonVersionString :: String
dickinsonVersionString = V.showVersion dickinsonVersion
