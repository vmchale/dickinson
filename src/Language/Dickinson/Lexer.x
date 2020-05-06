{
    module Language.Dickinson.Lexer ( alexMonadScan
                                    , runAlex
                                    , lexDickinson
                                    , AlexPosn (..)
                                    , Alex (..)
                                    , Token (..)
                                    , Keyword (..)
                                    , Sym (..)
                                    ) where

import Control.Arrow ((&&&))
import qualified Data.ByteString.Lazy as BSL
import Data.Functor (($>))
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Language.Dickinson.Name

}

%wrapper "monadUserState-bytestring"

$digit = [0-9]

@num = ($digit+ \. $digit+) | ($digit+)

$latin = [a-zA-Z]

@escape_str = \\ [\\\"]

-- single-line string
-- TODO: interpolations?
@string = \" ([^\"\\] | @escape_str)* \"

tokens :-

    <0> $white+                    ;
    <0> ";".*                      { tok (\p s -> alex $ TokLineComment p s) }

    -- assume utf8
    <0> $latin+                    { tok (\p s -> TokIdent p <$> newIdentAlex p (mkShort s)) }

    <0> \(                         { mkSym LParen }
    <0> \)                         { mkSym RParen }
    <0> \|                         { mkSym VBar }

    -- keywords
    <0> ":let"                     { mkKeyword KwLet }
    <0> ":branch"                  { mkKeyword KwBranch }
    <0> ":oneof"                   { mkKeyword KwOneof }
    <0> ":def"                     { mkKeyword KwDef }

    -- strings
    <0> @string                    { tok (\p s -> alex $ TokString p (mkShort s)) }

    -- numbers (as doubles)
    <0> @num                       { tok (\p s -> alex $ TokDouble p (read $ T.unpack (mkShort s))) }

{

mkShort :: BSL.ByteString -> T.Text
mkShort = decodeUtf8 . BSL.toStrict

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
        Just i -> (pre, Name t (Unique i) pos)
        Nothing -> let i = max' + 1
            in let newName = Name t (Unique i) pos
                in ((i, M.insert t i names, IM.insert i newName uniqs), newName)


alexInitUserState :: AlexUserState
alexInitUserState = (0, mempty, mempty)

data Sym = LParen
         | RParen
         | VBar
         deriving (Eq)

data Keyword = KwDef
             | KwLet
             | KwBranch
             | KwOneof
             deriving (Eq)

data Token a = EOF { loc :: a }
             | TokIdent { loc :: a, ident :: Name a }
             | TokDouble { loc :: a, double :: Double }
             | TokString { loc :: a, str :: T.Text }
             | TokMultilineString { loc :: a, str :: T.Text }
             | TokKeyword { loc :: a, kw :: Keyword }
             | TokLineComment { loc :: a, comment :: BSL.ByteString }
             | TokBlockComment { loc :: a, comment :: BSL.ByteString }
             | TokSym { loc :: a, sym :: Sym }
             deriving (Eq)

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

}
