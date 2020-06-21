module Main (main) where

import           Control.Exception                 (throw)
import           Control.Monad                     (void)
import           Criterion.Main
import           Data.Binary                       (decode, encode)
import qualified Data.ByteString.Lazy              as BSL
import           Language.Dickinson.Check
import           Language.Dickinson.DuplicateCheck
import           Language.Dickinson.File
import           Language.Dickinson.Lexer
import           Language.Dickinson.Parser
import           Language.Dickinson.Rename
import           Language.Dickinson.ScopeCheck
import           Language.Dickinson.Type
import           Language.Dickinson.Unique

main :: IO ()
main =
    defaultMain [ env parses $ \ ~(c, s) ->
                  bgroup "parse"
                    [ bench "lib/color.dck" $ nf parse c
                    , bench "lex lib/color.dck" $ nf lexDickinson c
                    , bench "examples/shakespeare.dck" $ nf parse s
                    ]
                , env libParsed $ \p ->
                  bgroup "renamer"
                    [ bench "bench/data/nestLet.dck" $ nf plainExpr p
                    ]
                , env (plainExpr <$> libParsed) $ \r ->
                  bgroup "scope checker"
                    [ bench "bench/data/nestLet.dck" $ nf checkScope r
                    ]
                , env (void <$> multiParsed) $ \p ->
                  bgroup "encoder"
                    [ bench "bench/data/multiple.dck" $ nf encode p
                    ]
                , env encoded $ \e ->
                  bgroup "decoder"
                    [ bench "bench/data/multiple.dck" $ nf (decode :: BSL.ByteString -> Dickinson ()) e
                    ]
                , env multiParsed $ \p ->
                  bgroup "check"
                    [ bench "bench/data/multiple.dck" $ nf checkMultiple p
                    , bench "bench/data/multiple.dck" $ nf checkDuplicates p -- TODO: better example
                    ]
                , bgroup "result"
                    [ bench "test/eval/context.dck" $ nfIO (evalFile [] "test/eval/context.dck")
                    , bench "examples/shakespeare.dck" $ nfIO (evalFile [] "examples/shakespeare.dck")
                    , bench "examples/doggo.dck" $ nfIO (evalFile ["prelude"] "examples/doggo.dck")
                    ]
                ]

    where libFile = BSL.readFile "lib/color.dck"
          shakespeare = BSL.readFile "examples/shakespeare.dck"
          parses = (,) <$> libFile <*> shakespeare
          libParsed = either throw id . parseWithMax <$> BSL.readFile "bench/data/nestLet.dck"
          multiParsed = either throw id . parse <$> BSL.readFile "bench/data/multiple.dck"
          encoded = encode . yeet <$> multiParsed

yeet :: Dickinson AlexPosn -> Dickinson ()
yeet = fmap void

plainExpr :: (UniqueCtx, Dickinson a) -> Dickinson a
plainExpr = fst . uncurry renameDickinson
