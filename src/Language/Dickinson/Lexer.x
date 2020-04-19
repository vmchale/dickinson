{
    module Language.Dickinson.Lexer ( alexMonadScan
                                    , runAlex
                                    , AlexPosn (..)
                                    , Alex (..)
                                    , LexerError
                                    ) where

import qualified Data.ByteString.Lazy as BSL
import Data.Text as T

}

%wrapper "monadUserState-bytestring"

$digit = [0-9]

@escape_str = \\ [\\\"]

-- single-line string
@string = \" ([^\"\\] | @escape_str)* \"

tokens :-

    <0> $white+                    ; 
    <0> "#".*                      { tok (\p s -> alex $ TokLineComment p s) }

    -- keywords
    <0> ":def"                     { mkKeyword Def }

{ 

data Keyword = Def

data Token a = EOF { loc :: a }
             | TokIdent { loc :: a, ident :: BSL.ByteString }
             | TokDouble { loc :: a, double :: Double }
             | TokString { loc :: a, str :: T.Text }
             | TokMultilineString { loc :: a, str :: T.Text }
             | TokKeyword { loc :: a, kw :: Keyword }
             | LineComment { loc :: a, comment :: BSL.ByteString }
             | BlockComment { loc :: a, comment :: BSL.ByteString }
             deriving (Eq)

}
