{

    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE TupleSections #-}
    module Language.Dickinson.Parser ( parse
                                     , parseWithCtx
                                     , ParseError (..)
                                     ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Prettyprint.Doc (Pretty (pretty), (<+>))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Dickinson.Lexer
import Language.Dickinson.Name hiding (loc)
import Language.Dickinson.Type
import Language.Dickinson.Unique

}

%name parseDickinson Dickinson
%tokentype { Token AlexPosn }
%error { parseError }
%monad { Parse } { (>>=) } { pure }
%lexer { lift alexMonadScan >>= } { EOF _ }

%token

    lparen { TokSym $$ LParen }
    rparen { TokSym $$ RParen }
    vbar { TokSym $$ VBar }
    lsqbracket { TokSym $$ LSqBracket }
    rsqbracket { TokSym $$ RSqBracket }

    beginInterp { TokSym $$ BeginInterp }
    endInterp { TokSym $$ EndInterp }

    def { TokKeyword $$ KwDef }
    let { TokKeyword $$ KwLet }
    branch { TokKeyword $$ KwBranch }
    oneof { TokKeyword $$ KwOneof }
    import { TokKeyword $$ KwImport }
    lambda { TokKeyword $$ KwLambda }

    ident { $$@(TokIdent _ _) }

    strChunk { $$@(TokStrChunk _ _) }
    stringLiteral { $$@(TokString _ _) }

    num { TokDouble _ $$ }

%%

many(p)
    : many(p) p { $2 : $1 }
    | { [] }

some(p)
    : many(p) p { $2 :| $1 }

parens(p)
    : lparen p rparen { $2 }

brackets(p)
    : lsqbracket p rsqbracket { $2 }

Dickinson :: { Dickinson Name AlexPosn }
          : many(parens(Declaration)) { reverse $1 }

Declaration :: { Declaration Name AlexPosn }
            : def Name parens(Expression) { Define $1 $2 $3 }
            | import Name { Import $1 $2 }

Name :: { Name AlexPosn }
     : ident { ident $1 }

Bind :: { (Name AlexPosn, Expression Name AlexPosn) }
     : Name Expression { ($1, $2) }

Interp :: { Expression Name AlexPosn }
Interp : strChunk { StrChunk (loc $1) (str $1) }
       | beginInterp Expression endInterp { $2 }

Expression :: { Expression Name AlexPosn }
           : branch some(parens(WeightedLeaf)) { Choice $1 (NE.reverse $2) }
           | oneof some(parens(Leaf)) { Choice $1 (NE.reverse (weight $2)) }
           | let some(brackets(Bind)) Expression { Let $1 (NE.reverse $2) $3 }
           | lambda Name parens(Expression) { Lambda $1 $2 $3 }
           | ident { Var (loc $1) (ident $1) }
           | stringLiteral { Literal (loc $1) (str $1) }
           | Expression Expression { Apply $1 $2 }
           | some(Interp) { Interp (toList $ NE.reverse $1) }
           | parens(Expression) { $1 }

WeightedLeaf :: { (Double, Expression Name AlexPosn) }
             : vbar num Expression { ($2, $3) }

Leaf :: { Expression Name AlexPosn }
     : vbar Expression { $2 }

{

weight :: NonEmpty (Expression name a) -> NonEmpty (Double, Expression name a)
weight es = (recip, ) <$> es
    where recip = 1 / (fromIntegral $ length es)

parseError :: Token AlexPosn -> Parse a
parseError = throwError . Unexpected

data ParseError a = Unexpected (Token a)
                  | LexErr String
                  deriving (Generic, NFData)

instance Pretty a => Pretty (ParseError a) where
    pretty (Unexpected tok)  = "Unexpected" <+> pretty tok <+> "at" <+> pretty (loc tok)
    pretty (LexErr str)      = pretty (T.pack str)

instance Pretty a => Show (ParseError a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (ParseError a)

type Parse = ExceptT (ParseError AlexPosn) Alex

parse :: BSL.ByteString -> Either (ParseError AlexPosn) (Dickinson Name AlexPosn)
parse = fmap snd . parseWithCtx

parseWithCtx :: BSL.ByteString -> Either (ParseError AlexPosn) (UniqueCtx, Dickinson Name AlexPosn)
parseWithCtx str = liftErr $ runAlexMax str (runExceptT parseDickinson)
    where liftErr (Left err)            = Left (LexErr err)
          liftErr (Right (_, Left err)) = Left err
          liftErr (Right (i, Right x))  = Right (i, x)

}
