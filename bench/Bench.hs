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
                , env (plainExpr <$> libParsed) $ \ ~(Dickinson _ r) ->
                  bgroup "scope checker"
                    [ bench "bench/data/nestLet.dck" $ nf checkScope r
                    ]
                , env (void <$> multiParsed) $ \p ->
                  bgroup "encoder"
                    [ bench "bench/data/multiple.dck" $ nf encode p
                    ]
                , env encodeEnv $ \ ~(e, es) ->
                  bgroup "decoder"
                    [ bench "bench/data/multiple.dck" $ nf (decode :: BSL.ByteString -> Dickinson ()) e
                    , bench "examples/shakespeare.dck" $ nf (decode :: BSL.ByteString -> Dickinson ()) es
                    ]
                , env multiParsed $ \ ~(Dickinson _ p) ->
                  bgroup "check"
                    [ bench "bench/data/multiple.dck" $ nf checkMultiple p
                    , bench "bench/data/multiple.dck" $ nf checkDuplicates p -- TODO: better example
                    ]
                , bgroup "result"
                    [ bench "test/eval/context.dck" $ nfIO (evalFile [] "test/eval/context.dck")
                    , bench "examples/shakespeare.dck" $ nfIO (evalFile [] "examples/shakespeare.dck")
                    , bench "examples/doggo.dck" $ nfIO (evalFile ["prelude"] "examples/doggo.dck")
                    , bench "test/demo/animal.dck" $ nfIO (evalFile ["lib"] "test/demo/animal.dck")
                    , bench "examples/hotTake.dck" $ nfIO (evalFile [] "examples/hotTake.dck")
                    , bench "examples/fortune.dck" $ nfIO (evalFile [] "examples/fortune.dck")
                    ]
                , bgroup "pipeline"
                    [ bench "examples/shakespeare.dck" $ nfIO (pipeline [] "examples/shakespeare.dck")
                    ]
                , bgroup "tcFile"
                    [ bench "examples/hotTake.dck" $ nfIO (tcFile [] "examples/hotTake.dck")
                    ]
                ]

    where libFile = BSL.readFile "lib/color.dck"
          shakespeare = BSL.readFile "examples/shakespeare.dck"
          parses = (,) <$> libFile <*> shakespeare
          libParsed = either throw id . parseWithMax <$> BSL.readFile "bench/data/nestLet.dck"
          multiParsed = either throw id . parse <$> BSL.readFile "bench/data/multiple.dck"
          encoded = encode . void <$> multiParsed
          encodeShakespeare = encode . void . either throw id . parse <$> shakespeare
          encodeEnv = (,) <$> encoded <*> encodeShakespeare

plainExpr :: (UniqueCtx, Dickinson a) -> Dickinson a
plainExpr = fst . uncurry renameDickinson
