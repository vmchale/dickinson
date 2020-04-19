{
    module Language.Dickinson.Parser ( parse
                                     , ParseError (..))
                                     ) where

import Language.Dickinson.Lexer

}

%name parseDickinson Dickinson
%tokentype { Token AlexPosn }
%error { parseError }
%monad { Parse } { (>>=) } { pure }
%lexer { lift alexMonadScan >>= } { EOF _ }

%token
    
    lparen { TokSpecial $$ LParen }
    rparen { TokSpecial $$ RParen }

    def { TokKeyword $$ Def }

%%

many(p)
    : many(p) p { $2 : $1 }
    | { [] }

some(p)
    : many(p) p { $2 :| $1 }

Dickinson :: { Dickinson AlexPosn }
          : many(Declaration) { $1 }

Declaration :: { Declaration AlexPosn }
            : def Name Expression { Define $1 $2 $3 }

{

parseError :: Token AlexPosn -> Parse a
parseError = throwError . Unexpected

data ParseError a = Unexpected (Token a)
                  | LexErr String
                  deriving (Generic, NFData)

type Parse = ExceptT (ParseError AlexPosn) Alex

parse :: BSL.ByteString -> Either (ParseError AlexPosn) (XATS AlexPosn)
parse str = liftErr $ runAlex str (runExceptT parseXATS)
    where liftErr (Left err) = Left (LexErr err)
          liftErr (Right (Left err)) = Left err
          liftErr (Right (Right x)) = Right x

}
