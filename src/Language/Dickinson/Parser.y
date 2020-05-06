{

    {-# LANGUAGE TupleSections #-}
    module Language.Dickinson.Parser ( parse
                                     , ParseError (..)
                                     ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as BSL
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Language.Dickinson.Lexer
import Language.Dickinson.Name hiding (loc)
import Language.Dickinson.Type

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

    def { TokKeyword $$ KwDef }
    let { TokKeyword $$ KwLet }
    branch { TokKeyword $$ KwBranch }
    oneof { TokKeyword $$ KwOneof }
    balance { TokKeyword $$ KwBalance }

    ident { $$@(TokIdent _ _) }

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
          : many(parens(Declaration)) { $1 }

Declaration :: { Declaration Name AlexPosn }
            : def Name parens(Expression) { Define $1 $2 $3 }

Name :: { Name AlexPosn }
     : ident { ident $1 }

Bind :: { (Name AlexPosn, Expression Name AlexPosn) }
     : Name Expression { ($1, $2) }

Expression :: { Expression Name AlexPosn }
           : stringLiteral { Literal (loc $1) (str $1) }
           | branch some(parens(WeightedLeaf)) { Choice $1 $2 }
           | oneof some(parens(Leaf)) { Choice $1 (weight $2) }
           | let some(brackets(Bind)) parens(Expression) { Let $1 $2 $3 }
           | ident { Var (loc $1) (ident $1) }

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

type Parse = ExceptT (ParseError AlexPosn) Alex

parse :: BSL.ByteString -> Either (ParseError AlexPosn) (Dickinson Name AlexPosn)
parse str = liftErr $ runAlex str (runExceptT parseDickinson)
    where liftErr (Left err) = Left (LexErr err)
          liftErr (Right (Left err)) = Left err
          liftErr (Right (Right x)) = Right x

}
