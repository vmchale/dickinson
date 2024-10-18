{

    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE OverloadedStrings #-}
    module Language.Dickinson.Parser ( parse
                                     , parseWithMax
                                     , parseWithCtx
                                     , parseWithInitCtx
                                     , parseReplWithCtx
                                     , parseExpressionWithCtx
                                     , ParseError (..)
                                     ) where

import Data.Bifunctor (first)
import Control.Composition ((.*))
import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Prettyprinter (Pretty (pretty), (<+>), concatWith, squotes)
import Data.Tuple.Ext (fst4)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Dickinson.Lexer
import Language.Dickinson.Name hiding (loc)
import Language.Dickinson.Probability
import Language.Dickinson.Type
import Language.Dickinson.Unique

}

%name parseDickinson Dickinson
%name parseExpression Expression
%name parseRepl DeclarationOrExpression
%tokentype { Token AlexPosn }
%error { parseError }
%error.expected
%monad { Parse } { (>>=) } { pure }
%lexer { lift alexMonadScan >>= } { EOF _ }

%token

    lparen { TokSym $$ LParen }
    rparen { TokSym $$ RParen }
    vbar { TokSym $$ VBar }
    lsqbracket { TokSym $$ LSqBracket }
    rsqbracket { TokSym $$ RSqBracket }
    rbracket { TokSym $$ RBracket }
    strBegin { TokSym $$ StrBegin }
    multiStrBegin { TokSym $$ MultiStrBegin }
    multiStrEnd { TokSym $$ MultiStrEnd }
    strEnd { TokSym $$ StrEnd }
    arrow { TokSym $$ Arrow }
    dollar { TokSym $$ DollarSign }
    comma { TokSym $$ Comma }
    underscore { TokSym $$ Underscore }
    colon { TokSym $$ Colon }
    declBreak { TokSym $$ DeclBreak }
    eq { TokSym $$ Eq }

    beginInterp { TokSym $$ BeginInterp }
    endInterp { TokSym $$ EndInterp }

    def { TokKeyword $$ KwDef }
    let { TokKeyword $$ KwLet }
    branch { TokKeyword $$ KwBranch }
    oneof { TokKeyword $$ KwOneof }
    include { TokKeyword $$ KwInclude }
    lambda { TokKeyword $$ KwLambda }
    match { TokKeyword $$ KwMatch }
    flatten { TokKeyword $$ KwFlatten }
    tydecl { TokKeyword $$ KwTyDecl }
    random { TokKeyword $$ KwRand }
    bind { TokKeyword $$ KwBind }

    builtin { $$@(TokBuiltin _ _) }

    text { TokKeyword $$ KwText }

    ident { $$@(TokIdent _ _) }
    tyIdent { $$@(TokTyCons _ _) }

    strChunk { $$@(TokStrChunk _ _) }
    stringLiteral { $$@(TokString _ _) }

    num { TokDouble _ $$ }

%%

many(p)
    : many(p) p { $2 : $1 }
    | { [] }

some(p)
    : many(p) p { $2 :| $1 }

sepBy(p,q)
    : sepBy(p,q) q p { $3 <| $1 }
    | p q p { $3 :| [$1] }

parens(p)
    : lparen p rparen { $2 }

brackets(p)
    : lsqbracket p rsqbracket { $2 }

Dickinson :: { Dickinson AlexPosn }
          : many(parens(Import)) declBreak many(Declaration) { Dickinson (reverse $1) (reverse $3) }

TyCons :: { NonEmpty (TyName AlexPosn) }
       : sepBy(tyIdent,vbar) { fmap tyIdent $1 }

Declaration :: { Declaration AlexPosn }
            : lparen def Name Expression rparen { Define $2 $3 $4 }
            | tydecl Name eq TyCons { TyDecl $1 $2 (NE.reverse $4) }

Import :: { Import AlexPosn }
       : include Name { Import $1 $2 }

Name :: { Name AlexPosn }
     : ident { ident $1 }

Type :: { DickinsonTy AlexPosn }
     : text { TyText $1 }
     | arrow Type Type { TyFun $1 $2 $3 }
     | lparen sepBy(Type,comma) rparen { TyTuple $1 (NE.reverse $2) }
     | ident { TyNamed (loc $1) (ident $1) }
     | parens(Type) { $1 }

Bind :: { (Name AlexPosn, Expression AlexPosn) }
     : Name Expression { ($1, $2) }

PatternBind :: { (Pattern AlexPosn, Expression AlexPosn) }
            : Pattern Expression { ($1, $2) }

Interp :: { Expression AlexPosn }
Interp : strChunk { StrChunk (loc $1) (str $1) }
       | beginInterp Expression endInterp { $2 }

Pattern :: { Pattern AlexPosn }
        : ident { PatternVar (loc $1) (ident $1) }
        | lparen sepBy(Pattern,comma) rparen { PatternTuple $1 (NE.reverse $2) }
        | underscore { Wildcard $1 }
        | tyIdent { PatternCons (loc $1) (tyIdent $1) }
        | lparen sepBy(Pattern,vbar) rparen { OrPattern $1 (NE.reverse $2) }

Expression :: { Expression AlexPosn }
           : branch some(parens(WeightedLeaf)) { Choice $1 (NE.reverse $2) }
           | oneof some(parens(Leaf)) { Choice $1 (NE.reverse (weight $2)) }
           | let some(brackets(Bind)) Expression { Let $1 (NE.reverse $2) $3 }
           | bind some(brackets(Bind)) Expression { Bind $1 (NE.reverse $2) $3 }
           | lambda Name Type Expression { Lambda $1 $2 $3 $4 }
           | ident { Var (loc $1) (ident $1) }
           | builtin { BuiltinFn (loc $1) (builtin $1) }
           | stringLiteral { Literal (loc $1) (str $1) }
           | strBegin some(Interp) strEnd { Interp $1 (toList $ NE.reverse $2) }
           | multiStrBegin some(Interp) multiStrEnd { MultiInterp $1 (processMultiChunks $ toList $ NE.reverse $2) }
           | rbracket many(Expression) { Concat $1 (reverse $2) }
           | dollar Expression Expression { Apply $1 $2 $3 }
           | lparen sepBy(Expression,comma) rparen { Tuple $1 (NE.reverse $2) }
           | match Expression some(brackets(PatternBind)) { Match $1 $2 (NE.reverse $3) }
           | flatten Expression { Flatten $1 $2 }
           | random ident { Random $1 (ident $2) }
           | Expression colon Type { Annot $2 $1 $3 }
           | tyIdent { Constructor (loc $1) (tyIdent $1) }
           | parens(Expression) { $1 }

WeightedLeaf :: { (Double, Expression AlexPosn) }
             : vbar num Expression { ($2, $3) }
             | num Expression { ($1, $2) }

Leaf :: { Expression AlexPosn }
     : vbar Expression { $2 }

DeclarationOrExpression :: { Either (Declaration AlexPosn) (Expression AlexPosn) }
                        : Expression { Right $1 }
                        | Declaration { Left $1 }

{

countSpaces :: T.Text -> Int
countSpaces = T.length . T.takeWhile (== ' ')

dropDoubleNewlines :: [T.Text] -> [T.Text]
dropDoubleNewlines ("":t:ts) | " " `T.isPrefixOf` t = dropDoubleNewlines (t:ts)
dropDoubleNewlines (t:ts)                           = t : dropDoubleNewlines ts
dropDoubleNewlines []                               = []

-- minimum indentation (relevant in a string containing newline characters)
minIndent :: T.Text -> Int
minIndent t = minimum (maxBound : fmap countSpaces (dropDoubleNewlines $ T.lines t)) -- reduce duplicates

minIndentExpr :: Expression a -> Maybe Int
minIndentExpr (StrChunk _ t) | "\n" `T.isInfixOf` t  = Just $ minIndent $ T.tail $ T.dropWhile (/= '\n') t -- tail because T.lines "\n   hello" is ["", "    hello"]
minIndentExpr _                                      = Nothing

mapStrChunk :: (T.Text -> T.Text) -> Expression a -> Expression a
mapStrChunk f (StrChunk l t) = StrChunk l (f t)
mapStrChunk _ e              = e

minIndentChunks :: [Expression a] -> Int
minIndentChunks es =
    minimum (maxBound : mapMaybe minIndentExpr es)

processMultiChunks :: [Expression a] -> [Expression a]
processMultiChunks es = {-# SCC "processMultiChunks" #-}
    let toStrip = minIndentChunks es
        in let needle = "\n" <> T.replicate toStrip " "
            in mapStrChunk (T.replace needle "\n") <$> es

parseError :: Token AlexPosn -> [String] -> Parse a
parseError = throwError .* Unexpected

data ParseError a = Unexpected (Token a) [String]
                  | LexErr String
                  deriving (Generic, NFData)

instance Pretty a => Pretty (ParseError a) where
    pretty (Unexpected tok valid) = pretty (loc tok) <+> "Unexpected" <+> pretty tok <> "." <+> "Expected one of" <+> concatWith (\x y -> x <> "," <+> y) (squotes.pretty<$>valid)
    pretty (LexErr str)           = pretty (T.pack str)

instance Pretty a => Show (ParseError a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (ParseError a)

type Parse = ExceptT (ParseError AlexPosn) Alex

parse :: BSL.ByteString -> Either (ParseError AlexPosn) (Dickinson AlexPosn)
parse = fmap snd . parseWithMax

parseReplWithCtx :: BSL.ByteString -> AlexUserState -> Either (ParseError AlexPosn) (AlexUserState, Either (Declaration AlexPosn) (Expression AlexPosn))
parseReplWithCtx = parseWithInitSt parseRepl

parseExpressionWithCtx :: BSL.ByteString -> AlexUserState -> Either (ParseError AlexPosn) (AlexUserState, Expression AlexPosn)
parseExpressionWithCtx = parseWithInitSt parseExpression

parseWithInitCtx :: BSL.ByteString -> Either (ParseError AlexPosn) (AlexUserState, Dickinson AlexPosn)
parseWithInitCtx bsl = parseWithCtx bsl alexInitUserState

parseWithCtx :: BSL.ByteString -> AlexUserState -> Either (ParseError AlexPosn) (AlexUserState, Dickinson AlexPosn)
parseWithCtx = parseWithInitSt parseDickinson

parseWithMax :: BSL.ByteString -> Either (ParseError AlexPosn) (UniqueCtx, Dickinson AlexPosn)
parseWithMax = parseWrapper parseDickinson

parseWithInitSt :: Parse a -> BSL.ByteString -> AlexUserState -> Either (ParseError AlexPosn) (AlexUserState, a)
parseWithInitSt parser str st = liftErr $ withAlexSt str st (runExceptT parser)
    where liftErr (Left err)            = Left (LexErr err)
          liftErr (Right (_, Left err)) = Left err
          liftErr (Right (i, Right x))  = Right (i, x)

parseWrapper :: Parse a -> BSL.ByteString -> Either (ParseError AlexPosn) (UniqueCtx, a)
parseWrapper parser str = fmap (first fst4) $ liftErr $ runAlexSt str (runExceptT parser)

liftErr :: Either String (b, Either (ParseError a) c) -> Either (ParseError a) (b, c)
liftErr (Left err)            = Left (LexErr err)
liftErr (Right (_, Left err)) = Left err
liftErr (Right (i, Right x))  = Right (i, x)

}
