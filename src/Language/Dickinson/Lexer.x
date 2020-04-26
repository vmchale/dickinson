{
    module Language.Dickinson.Lexer ( alexMonadScan
                                    , runAlex
                                    , AlexPosn (..)
                                    , Alex (..)
                                    , Token (..)
                                    , Keyword (..)
                                    , Sym (..)
                                    ) where

import Control.Arrow ((&&&))
import qualified Data.ByteString.Lazy as BSL
import Data.Text as T

}

%wrapper "monad-bytestring"

$digit = [0-9]

@escape_str = \\ [\\\"]

-- single-line string
-- TODO: interpolations?
@string = \" ([^\"\\] | @escape_str)* \"

tokens :-

    <0> $white+                    ; 
    <0> ";".*                      { tok (\p s -> alex $ TokLineComment p s) }

    <0> \(                         { mkSym LParen }
    <0> \)                         { mkSym RParen }

    -- keywords
    <0> ":let"                     { mkKeyword KwLet }
    <0> ":branch"                  { mkKeyword KwBranch }
    <0> ":oneof"                   { mkKeyword KwOneof }

{ 

alex :: a -> Alex a
alex = pure

gets_alex :: (AlexState -> a) -> Alex a
gets_alex f = Alex (Right . (id &&& f))

get_pos :: Alex AlexPosn
get_pos = gets_alex alex_pos

alexEOF = EOF <$> get_pos

tok f (p,_,s,_) len = f p (BSL.take len s)

constructor c t = tok (\p _ -> alex $ c p t)

mkKeyword = constructor TokKeyword

mkSym = constructor TokSym

data Sym = LParen
         | RParen
         deriving (Eq)

data Keyword = KwDef
             | KwLet
             | KwBranch
             | KwOneof
             deriving (Eq)

data Token a = EOF { loc :: a }
             | TokIdent { loc :: a, ident :: BSL.ByteString }
             | TokDouble { loc :: a, double :: Double }
             | TokString { loc :: a, str :: T.Text }
             | TokMultilineString { loc :: a, str :: T.Text }
             | TokKeyword { loc :: a, kw :: Keyword }
             | TokLineComment { loc :: a, comment :: BSL.ByteString }
             | TokBlockComment { loc :: a, comment :: BSL.ByteString }
             | TokSym { loc :: a, sym :: Sym }
             deriving (Eq)

}
