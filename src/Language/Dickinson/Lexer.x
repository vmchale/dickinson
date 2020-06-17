{
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE StandaloneDeriving #-}
    module Language.Dickinson.Lexer ( alexMonadScan
                                    , runAlex
                                    , runAlexMax
                                    , lexDickinson
                                    , AlexPosn (..)
                                    , Alex (..)
                                    , Token (..)
                                    , Keyword (..)
                                    , Sym (..)
                                    ) where

import Control.Arrow ((&&&))
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as ASCII
import Data.Functor (($>))
import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Prettyprint.Doc (Pretty (pretty), pipe, lparen, rparen, rbrace, rbracket, lbracket, colon, dquotes, dquote)
import GHC.Generics (Generic)
import Language.Dickinson.Name
import Language.Dickinson.Unique

}

%wrapper "monadUserState-bytestring"

$digit = [0-9]

@num = ($digit+ \. $digit+) | ($digit+)

$latin = [a-zA-Z]

$str_special = [\\\"\$]

@escape_str = \\ $str_special

-- single-line string
-- TODO: interpolations?
@string = \" ([^ $str_special] | @escape_str)* \"

$str_chunk = [^ \"\\\$]

@interp = \$\{

@name = ($latin+ \.)* $latin+

tokens :-

    <0> $white+                    ;
    <0> ";".*                      ;

    <0> \(                         { mkSym LParen }
    <0> \)                         { mkSym RParen }
    <0> \|                         { mkSym VBar }
    <0> \[                         { mkSym LSqBracket }
    <0> \]                         { mkSym RSqBracket }
    <0> "⟶"                        { mkSym Arrow }
    <0> "->"                       { mkSym Arrow }

    -- keywords
    <0> ":let"                     { mkKeyword KwLet }
    <0> ":branch"                  { mkKeyword KwBranch }
    <0> ":oneof"                   { mkKeyword KwOneof }
    <0> ":def"                     { mkKeyword KwDef }
    <0> ":import"                  { mkKeyword KwImport }
    <0> ":lambda"                  { mkKeyword KwLambda }
    <0> "text"                     { mkKeyword KwText }

    <0> @name                      { tok (\p s -> TokIdent p <$> newIdentAlex p (mkText s)) }

    -- TODO: StrBegin token?
    -- strings
    <0> \"                         { mkSym StrBegin `andBegin` string }
    <string> $str_chunk+           { tok (\p s -> alex $ TokStrChunk p (mkText s)) }
    <string> @interp               { mkSym BeginInterp `andBegin` 0 }
    <0> \}                         { mkSym EndInterp `andBegin` string }
    <string> \"                    { mkSym StrEnd `andBegin` 0 }

    <0> "'''"                      { mkSym MultiStrBegin `andBegin` multiString }
    <multiString> @interp          { mkSym BeginInterp `andBegin` 0 }
    <multiString> "''''"           { mkSym MultiStrEnd `andBegin` 0 }

    -- strings
    <0> @string                    { tok (\p s -> alex $ TokString p (T.tail . T.init $ mkText s)) }

    -- numbers (as doubles)
    <0> @num                       { tok (\p s -> alex $ TokDouble p (read $ ASCII.unpack s)) } -- shouldn't cause any problems cuz digits

{

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

-- "inside out - track ints by name!"
type AlexUserState = (Int, M.Map T.Text Int, NameEnv AlexPosn)

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
         | BeginInterp
         | EndInterp
         | StrBegin
         | StrEnd
         | Arrow
         | MultiStrBegin
         | MultiStrEnd
         deriving (Eq, Generic, NFData)

instance Pretty Sym where
    pretty LParen        = lparen
    pretty RParen        = rparen
    pretty VBar          = pipe
    pretty LSqBracket    = lbracket
    pretty RSqBracket    = rbracket
    pretty BeginInterp   = "${"
    pretty EndInterp     = rbrace
    pretty StrBegin      = dquote
    pretty StrEnd        = dquote
    pretty Arrow         = "⟶"
    pretty MultiStrBegin = "'''"
    pretty MultiStrEnd   = "'''"

data Keyword = KwDef
             | KwLet
             | KwBranch
             | KwOneof
             | KwImport
             | KwLambda
             | KwText
             deriving (Eq, Generic, NFData)

instance Pretty Keyword where
    pretty KwDef    = ":def"
    pretty KwLet    = ":let"
    pretty KwBranch = ":branch"
    pretty KwOneof  = ":oneof"
    pretty KwImport = ":import"
    pretty KwLambda = ":lambda"
    pretty KwText   = "text"

instance Pretty AlexPosn where
    pretty (AlexPn _ line col) = pretty line <> colon <> pretty col

deriving instance Generic AlexPosn

deriving instance NFData AlexPosn

data Token a = EOF { loc :: a }
             | TokIdent { loc :: a, ident :: Name a }
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
    pretty (TokDouble _ d)      = pretty d
    pretty (TokString _ str')   = dquotes (pretty str')
    pretty (TokStrChunk _ str') = pretty str'
    pretty (TokKeyword _ kw')   = pretty kw'
    pretty (TokSym _ sym')      = pretty sym'

-- for testing
loop :: Alex [Token AlexPosn]
loop = do
    tok' <- alexMonadScan
    case tok' of
        EOF{} -> pure []
        _ -> (tok' :) <$> loop

-- | N.B. for testing
lexDickinson :: BSL.ByteString -> Either String [Token AlexPosn]
lexDickinson = flip runAlex loop

runAlexMax :: BSL.ByteString -> Alex a -> Either String (Int, a)
runAlexMax = (fmap (first fst3) .) . (\inp -> withAlexSt inp alexInitUserState)
    where fst3 (x, _, _) = x

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
