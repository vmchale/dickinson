{
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE StandaloneDeriving #-}
    module Language.Dickinson.Lexer ( alexMonadScan
                                    , runAlex
                                    , runAlexSt
                                    , withAlexSt
                                    , lexDickinson
                                    , alexInitUserState
                                    , AlexPosn (..)
                                    , AlexUserState
                                    , Alex (..)
                                    , Token (..)
                                    , Keyword (..)
                                    , Sym (..)
                                    , HasLexerState (..)
                                    ) where

import Control.Arrow ((&&&))
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Binary (Binary)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as ASCII
import Data.Functor (($>))
import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Prettyprint.Doc (Pretty (pretty), pipe, lparen, rparen, rbrace, rbracket, lbracket, colon, dquotes, dquote, rangle, comma)
import GHC.Generics (Generic)
import Language.Dickinson.Name
import Language.Dickinson.Unique
import Lens.Micro (Lens')

}

%wrapper "monadUserState-bytestring"

$digit = [0-9]

@num = ($digit+ \. $digit+) | ($digit+)

$latin = [a-zA-Z]

$str_special = [\\\"\$]

@escape_str = \\ [$str_special \$n]

-- single-line string
@string = \" ([^ $str_special] | @escape_str)* \"

$str_chunk = [^ \"\\\$]

@escape_str_chunk = \\ [$str_chunk \$n]
@str_interp_in = ([$str_chunk] | @escape_str_chunk)+

@interp = \$\{

@follow_char = [$latin $digit]

@name = ([a-z] @follow_char* \.)* @follow_char+
@tyname = ([A-Z] @follow_char* \.)* @follow_char+

tokens :-

    <0> $white+                    ;
    <0> ";".*                      ;

    <0> \(                         { mkSym LParen }
    <0> \)                         { mkSym RParen }
    <0> \>                         { mkSym RBracket }
    <0> \|                         { mkSym VBar }
    <0> \[                         { mkSym LSqBracket }
    <0> \]                         { mkSym RSqBracket }
    <0> \$                         { mkSym DollarSign }
    <0> \,                         { mkSym Comma }
    <0> \_                         { mkSym Underscore }
    <0> "⟶"                        { mkSym Arrow }
    <0> "->"                       { mkSym Arrow }
    <0> \:                         { mkSym Colon }
    <0> \%\-                       { mkSym DeclBreak }
    <0> "="                        { mkSym Eq }

    -- keywords
    <0> ":let"                     { mkKeyword KwLet }
    <0> ":branch"                  { mkKeyword KwBranch }
    <0> ":oneof"                   { mkKeyword KwOneof }
    <0> ":def"                     { mkKeyword KwDef }
    <0> ":include"                 { mkKeyword KwInclude }
    <0> ":lambda"                  { mkKeyword KwLambda }
    <0> "text"                     { mkKeyword KwText }
    <0> ":match"                   { mkKeyword KwMatch }
    <0> ":flatten"                 { mkKeyword KwFlatten }
    <0> "tydecl"                   { mkKeyword KwTyDecl }

    <0> @name                      { tok (\p s -> TokIdent p <$> newIdentAlex p (mkText s)) }
    <0> @tyname                    { tok (\p s -> TokTyCons p <$> newIdentAlex p (mkText s)) }

    -- strings
    <0> \"                         { mkSym StrBegin `andBegin` string }
    <string> @str_interp_in        { tok (\p s -> alex $ TokStrChunk p (escReplace $ mkText s)) }
    <string> @interp               { mkSym BeginInterp `andBegin` 0 }
    <0> \}                         { mkSym EndInterp `andBegin` string }
    <string> \"                    { mkSym StrEnd `andBegin` 0 }

    -- strings
    <0> @string                    { tok (\p s -> alex $ TokString p (escReplace . T.tail . T.init $ mkText s)) }

    -- numbers (as doubles)
    <0> @num                       { tok (\p s -> alex $ TokDouble p (read $ ASCII.unpack s)) } -- shouldn't cause any problems cuz digits

{

escReplace :: T.Text -> T.Text
escReplace =
      T.replace "\\\"" "\""
    . T.replace "\\n" "\n"
    . T.replace "\\$" "$"

mkText :: BSL.ByteString -> T.Text
mkText = decodeUtf8 . BSL.toStrict

alex :: a -> Alex a
alex = pure

set_ust :: AlexUserState -> Alex ()
set_ust st = Alex (Right . (go &&& (const ())))
    where go s = s { alex_ust = st }

gets_alex :: (AlexState -> a) -> Alex a
gets_alex f = Alex (Right . (id &&& f))

get_ust :: Alex AlexUserState
get_ust = gets_alex alex_ust

get_pos :: Alex AlexPosn
get_pos = gets_alex alex_pos

alexEOF = EOF <$> get_pos

tok f (p,_,s,_) len = f p (BSL.take len s)

constructor c t = tok (\p _ -> alex $ c p t)

mkKeyword = constructor TokKeyword

mkSym = constructor TokSym

type AlexUserState = (UniqueCtx, M.Map T.Text Int, NameEnv AlexPosn)

class HasLexerState a where
    lexerStateLens :: Lens' a AlexUserState

newIdentAlex :: AlexPosn -> T.Text -> Alex (Name AlexPosn)
newIdentAlex pos t = do
    st <- get_ust
    let (st', n) = newIdent pos t st
    set_ust st' $> (n $> pos)

newIdent :: AlexPosn -> T.Text -> AlexUserState -> (AlexUserState, Name AlexPosn)
newIdent pos t pre@(max', names, uniqs) =
    case M.lookup t names of
        Just i -> (pre, Name tQual (Unique i) pos)
        Nothing -> let i = max' + 1
            in let newName = Name tQual (Unique i) pos
                in ((i, M.insert t i names, IM.insert i newName uniqs), newName)
    where tQual = NE.fromList (T.splitOn "." t)

alexInitUserState :: AlexUserState
alexInitUserState = (0, mempty, mempty)

data Sym = LParen
         | RParen
         | VBar
         | LSqBracket
         | RSqBracket
         | RBracket
         | BeginInterp
         | EndInterp
         | StrBegin
         | StrEnd
         | Arrow
         | DollarSign
         | Comma
         | Underscore
         | Colon
         | DeclBreak
         | Eq
         deriving (Eq, Generic, NFData)

instance Pretty Sym where
    pretty LParen        = lparen
    pretty RParen        = rparen
    pretty VBar          = pipe
    pretty LSqBracket    = lbracket
    pretty RSqBracket    = rbracket
    pretty RBracket      = rangle
    pretty BeginInterp   = "${"
    pretty EndInterp     = rbrace
    pretty StrBegin      = dquote
    pretty StrEnd        = dquote
    pretty Arrow         = "⟶"
    pretty DollarSign    = "$"
    pretty Comma         = comma
    pretty Underscore    = "_"
    pretty Colon         = colon
    pretty DeclBreak     = "%-"
    pretty Eq            = "="

data Keyword = KwDef
             | KwLet
             | KwBranch
             | KwOneof
             | KwInclude
             | KwLambda
             | KwText
             | KwMatch
             | KwFlatten
             | KwTyDecl
             deriving (Eq, Generic, NFData)

instance Pretty Keyword where
    pretty KwDef     = ":def"
    pretty KwLet     = ":let"
    pretty KwBranch  = ":branch"
    pretty KwOneof   = ":oneof"
    pretty KwInclude = ":include"
    pretty KwLambda  = ":lambda"
    pretty KwText    = "text"
    pretty KwMatch   = ":match"
    pretty KwFlatten = ":flatten"
    pretty KwTyDecl  = "tydecl"

instance Pretty AlexPosn where
    pretty (AlexPn _ line col) = pretty line <> colon <> pretty col

deriving instance Generic AlexPosn

deriving instance NFData AlexPosn

deriving instance Binary AlexPosn

data Token a = EOF { loc :: a }
             | TokIdent { loc :: a, ident :: Name a }
             | TokTyCons { loc :: a, tyIdent :: TyName a }
             | TokDouble { loc :: a, double :: Double }
             -- separate tok for full strings for sake of speed
             | TokString { loc :: a, str :: T.Text }
             | TokStrChunk { loc :: a, str :: T.Text }
             | TokKeyword { loc :: a, kw :: Keyword }
             | TokSym { loc :: a, sym :: Sym }
             deriving (Eq, Generic, NFData)

instance Pretty (Token a) where
    pretty EOF{}                = mempty
    pretty (TokIdent _ n)       = pretty n
    pretty (TokTyCons _ tn)     = pretty tn
    pretty (TokDouble _ d)      = pretty d
    pretty (TokString _ str')   = dquotes (pretty str')
    pretty (TokStrChunk _ str') = pretty str'
    pretty (TokKeyword _ kw')   = pretty kw'
    pretty (TokSym _ sym')      = pretty sym'

loop :: Alex [Token AlexPosn]
loop = do
    tok' <- alexMonadScan
    case tok' of
        EOF{} -> pure []
        _ -> (tok' :) <$> loop

lexDickinson :: BSL.ByteString -> Either String [Token AlexPosn]
lexDickinson = flip runAlex loop

runAlexSt :: BSL.ByteString -> Alex a -> Either String (AlexUserState, a)
runAlexSt inp = withAlexSt inp alexInitUserState

withAlexSt :: BSL.ByteString -> AlexUserState -> Alex a -> Either String (AlexUserState, a)
withAlexSt inp ust (Alex f) = first alex_ust <$> f
    (AlexState { alex_bpos = 0
               , alex_pos = alexStartPos
               , alex_inp = inp
               , alex_chr = '\n'
               , alex_ust = ust
               , alex_scd = 0
               })

}
