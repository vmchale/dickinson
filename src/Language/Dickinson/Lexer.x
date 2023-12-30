{
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE DeriveDataTypeable #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE StandaloneDeriving #-}
    module Language.Dickinson.Lexer ( alexMonadScan
                                    , runAlex
                                    , runAlexSt
                                    , withAlexSt
                                    , lexDickinson
                                    , alexInitUserState
                                    , scdInitState
                                    , AlexPosn (..)
                                    , AlexUserState
                                    , Alex (..)
                                    , Token (..)
                                    , Keyword (..)
                                    , Sym (..)
                                    , Builtin (..)
                                    , HasLexerState (..)
                                    ) where

import Control.Arrow ((&&&))
import Data.Data (Data)
import Control.DeepSeq (NFData)
import Control.Monad.Fail (MonadFail (..))
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
import Prettyprinter (Pretty (pretty), pipe, lparen, rparen, rbrace, rbracket, lbracket, colon, squotes, dquotes, dquote, rangle, comma, (<+>))
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

@escape_str = \\ [$str_special n]

-- single-line string
@string = \" ([^ $str_special] | "$" [^\{\"\$] | @escape_str)* (\" | \$\")

$str_chunk = [^\"\\\$]

@str_interp_in = ($str_chunk | @escape_str)+

@interp = \$\{

@follow_char = [$latin $digit]

@lower_name = [a-z] @follow_char*
@name = (@lower_name \.)* @lower_name
@tyname = [A-Z] @follow_char*

@multi_str_in = ([^\'\$] | $white)*

tokens :-

    <0> "#!".*                     ; -- ignore shebangs
    <0> {

        $white+                    ;
        ";".*                      ;

        \(                         { mkSym LParen }
        \)                         { mkSym RParen }
        \>                         { mkSym RBracket }
        \|                         { mkSym VBar }
        \[                         { mkSym LSqBracket }
        \]                         { mkSym RSqBracket }
        \$                         { mkSym DollarSign }
        \,                         { mkSym Comma }
        \_                         { mkSym Underscore }
        "⟶"                        { mkSym Arrow }
        "->"                       { mkSym Arrow }
        \:                         { mkSym Colon }
        \%\-                       { mkSym DeclBreak }
        "="                        { mkSym Eq }

        -- keywords
        ":let"                     { mkKeyword KwLet }
        ":branch"                  { mkKeyword KwBranch }
        ":oneof"                   { mkKeyword KwOneof }
        ":def"                     { mkKeyword KwDef }
        ":include"                 { mkKeyword KwInclude }
        ":lambda"                  { mkKeyword KwLambda }
        "text"                     { mkKeyword KwText }
        ":match"                   { mkKeyword KwMatch }
        ":flatten"                 { mkKeyword KwFlatten }
        "tydecl"                   { mkKeyword KwTyDecl }
        ":pick"                    { mkKeyword KwRand }
        ":bind"                    { mkKeyword KwBind }

        -- builtins
        capitalize                 { mkBuiltin Capitalize }
        allCaps                    { mkBuiltin AllCaps }
        titlecase                  { mkBuiltin Titlecase }
        oulipo                     { mkBuiltin Oulipo }

        -- identifiers
        @name                      { tok (\p s -> TokIdent p <$> newIdentAlex p (mkText s)) }
        @tyname                    { tok (\p s -> TokTyCons p <$> newIdentAlex p (mkText s)) }

    }

    -- interpolated strings
    <0> \"                         { doSym StrBegin (pushScd InStr) `andBegin` string }
    <string> @str_interp_in        { tok (\p s -> alex $ TokStrChunk p (escReplace $ mkText s)) }
    <string,multiStr> @interp      { mkSym BeginInterp `andBegin` 0 }
    <string,multiStr> "$"          { tok (\p s -> alex $ TokStrChunk p (mkText s)) }
    <0> \}                         { doSym EndInterp exitInterp }
    <string> \"                    { doSym StrEnd popScd `andBegin` 0 }

    <0> "'''"                      { doSym MultiStrBegin (pushScd InMultiStr) `andBegin` multiStr }
    <multiStr> @multi_str_in       { tok (\p s -> alex $ TokStrChunk p (mkText s)) }
    <multiStr> \' [^\']            { tok (\p s -> alex $ TokStrChunk p (mkText s)) }
    <multiStr> "'''"               { doSym MultiStrEnd popScd `andBegin` 0 }

    -- strings
    <0> @string                    { tok (\p s -> alex $ TokString p (escReplace . T.tail . T.init $ mkText s)) }

    -- numbers (as doubles)
    <0> @num                       { tok (\p s -> alex $ TokDouble p (read $ ASCII.unpack s)) } -- shouldn't cause any problems cuz digits

{

instance MonadFail Alex where
    fail = alexError

escReplace :: T.Text -> T.Text
escReplace =
      T.replace "\\\"" "\""
    . T.replace "\\n" "\n"
    . T.replace "\\$" "$"

mkText :: BSL.ByteString -> T.Text
mkText = {-# SCC "mkText" #-} decodeUtf8 . BSL.toStrict

alex :: a -> Alex a
alex = pure

popScd :: Alex ()
popScd = mod_ust (\(x,pop,y,z) -> (x,tail pop,y,z))

pushScd :: ScdState -> Alex ()
pushScd st =
    mod_ust (\(x,push,y,z) -> (x,st:push,y,z))

exitInterp :: Alex ()
exitInterp = do
    (_,iSt:_,_,_) <- alexGetUserState
    case iSt of
        InStr -> set_scd string
        InMultiStr -> set_scd multiStr

set_scd :: Int -> Alex ()
set_scd st = Alex (Right . (go &&& (const ())))
    where go s = s { alex_scd = st }

set_ust :: AlexUserState -> Alex ()
set_ust st = Alex (Right . (go &&& (const ())))
    where go s = s { alex_ust = st }

mod_ust :: (AlexUserState -> AlexUserState) -> Alex ()
mod_ust f = Alex (Right . (go &&& (const ())))
    where go s = s { alex_ust = f (alex_ust s) }

gets_alex :: (AlexState -> a) -> Alex a
gets_alex f = Alex (Right . (id &&& f))

get_pos :: Alex AlexPosn
get_pos = gets_alex alex_pos

alexEOF = EOF <$> get_pos

tok f (p,_,s,_) len = f p (BSL.take len s)

constructor c t = tok (\p _ -> alex $ c p t)

mkKeyword = constructor TokKeyword

mkBuiltin = constructor TokBuiltin

mkSym = constructor TokSym

doSym t act = tok (\p _ -> TokSym p t <$ act)

-- | Used to track nesting level for string interpolations
data ScdState = InStr
              | InMultiStr

type AlexUserState = (UniqueCtx, [ScdState], M.Map T.Text Int, NameEnv AlexPosn)

class HasLexerState a where
    lexerStateLens :: Lens' a AlexUserState

newIdentAlex :: AlexPosn -> T.Text -> Alex (Name AlexPosn)
newIdentAlex pos t = do
    st <- alexGetUserState
    let (st', n) = newIdent pos t st
    alexSetUserState st' $> (n $> pos)

newIdent :: AlexPosn -> T.Text -> AlexUserState -> (AlexUserState, Name AlexPosn)
newIdent pos t pre@(max', scd, names, uniqs) = {-# SCC "newIdent" #-}
    case M.lookup t names of
        Just i -> (pre, Name tQual (Unique i) pos)
        Nothing -> let i = max' + 1
            in let newName = Name tQual (Unique i) pos
                in ((i, scd, M.insert t i names, IM.insert i newName uniqs), newName)
    where tQual = NE.fromList (T.splitOn "." t)

scdInitState :: [ScdState]
scdInitState = mempty

alexInitUserState :: AlexUserState
alexInitUserState = (0, scdInitState, mempty, mempty)

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
         | MultiStrBegin
         | MultiStrEnd
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
    pretty MultiStrBegin = "'''"
    pretty MultiStrEnd   = "'''"
    pretty Arrow         = "⟶"
    pretty DollarSign    = "$"
    pretty Comma         = comma
    pretty Underscore    = "_"
    pretty Colon         = colon
    pretty DeclBreak     = "%-"
    pretty Eq            = "="

data Builtin = Capitalize
             | AllCaps
             | Titlecase
             | Oulipo -- ^ Filter all @e@s from text.
             deriving (Eq, Show, Generic, NFData, Binary, Data)

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
             | KwRand
             | KwBind
             deriving (Eq, Generic, NFData)

instance Pretty Builtin where
    pretty Capitalize = "capitalize"
    pretty AllCaps    = "allCaps"
    pretty Titlecase  = "titlecase"
    pretty Oulipo     = "oulipo"

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
    pretty KwRand    = ":pick"
    pretty KwBind    = ":bind"

instance Pretty AlexPosn where
    pretty (AlexPn _ line col) = pretty line <> colon <> pretty col

deriving instance Generic AlexPosn

deriving instance NFData AlexPosn

deriving instance Binary AlexPosn

deriving instance Data AlexPosn

data Token a = EOF { loc :: a }
             | TokIdent { loc :: a, ident :: Name a }
             | TokTyCons { loc :: a, tyIdent :: TyName a }
             | TokDouble { loc :: a, double :: Double }
             | TokStrChunk { loc :: a, str :: T.Text }
             -- separate tok for full strings for sake of speed
             | TokString { loc :: a, str :: T.Text }
             | TokKeyword { loc :: a, kw :: Keyword }
             | TokSym { loc :: a, sym :: Sym }
             | TokBuiltin { loc :: a, builtin :: Builtin }
             deriving (Eq, Generic, NFData)

instance Pretty (Token a) where
    pretty EOF{}                = mempty
    pretty (TokIdent _ n)       = "identifier" <+> squotes (pretty n)
    pretty (TokTyCons _ tn)     = "constructor" <+> squotes (pretty tn)
    pretty (TokDouble _ d)      = pretty d
    pretty (TokString _ str')   = dquotes (pretty str')
    pretty (TokStrChunk _ str') = pretty str'
    pretty (TokKeyword _ kw')   = "keyword" <+> squotes (pretty kw')
    pretty (TokSym _ sym')      = pretty sym'
    pretty (TokBuiltin _ b)     = "builtin" <+> squotes (pretty b)

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
