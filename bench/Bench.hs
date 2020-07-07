module Main (main) where

import           Control.Exception                 (throw)
import           Control.Exception.Value           (eitherThrow)
import           Control.Monad                     (void)
import           Criterion.Main
import           Data.Binary                       (decode, encode)
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.Text                         as T
import           Language.Dickinson.Check
import           Language.Dickinson.DuplicateCheck
import           Language.Dickinson.Eval
import           Language.Dickinson.File
import           Language.Dickinson.Lexer
import           Language.Dickinson.Parser
import           Language.Dickinson.Pipeline
import           Language.Dickinson.Rename
import           Language.Dickinson.ScopeCheck
import           Language.Dickinson.Type
import           Language.Dickinson.TypeCheck
import           Language.Dickinson.Unique

benchResult :: FilePath -> Benchmark
benchResult fp = bench fp $ nfIO (evalFile [] fp)

benchPipeline :: FilePath -> Benchmark
benchPipeline fp = bench fp $ nfIO (pipeline [] fp)

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
                    [ benchResult "test/eval/context.dck"
                    , benchResult "examples/shakespeare.dck"
                    , bench "examples/doggo.dck" $ nfIO (evalFile ["prelude"] "examples/doggo.dck")
                    , bench "test/demo/animal.dck" $ nfIO (evalFile ["lib"] "test/demo/animal.dck")
                    , benchResult "examples/fortune.dck"
                    ]
                , bgroup "pipeline"
                    [ benchPipeline "examples/shakespeare.dck"
                    , benchPipeline "examples/fortune.dck"
                    , benchPipeline "examples/catherineOfSienaBot.dck"
                    ]
                , bgroup "tcFile"
                    [ bench "examples/fortune.dck" $ nfIO (tcFile [] "examples/fortune.dck") -- TODO: tc with syntax tree in env?
                    ]
                , env amalFortune $ \f ->
                  bgroup "typecheck"
                    [ bench "examples/fortune.dck" $ nf tyRun f ]
                , env amalgamated $ \ ~(s, c) ->
                  bgroup "check + eval"
                    [ bench "examples/shakespeare.dck" $ nfIO (txtIO s)
                    , bench "examples/catherineOfSienaBot.dck" $ nfIO (txtIO c)
                    ]
                , env amalgamated $ \ ~(s, c) ->
                  bgroup "eval"
                    [ bench "examples/shakespeare.dck" $ nfIO (evalIO $ evalDickinsonAsMain s)
                    , bench "examples/catherineOfSienaBot.dck" $ nfIO (evalIO $ evalDickinsonAsMain c)
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
          amalFortune = amalgamateRename [] "examples/fortune.dck"
          amalgamated = (,)
            <$> amalgamateRename [] "examples/shakespeare.dck"
            <*> amalgamateRename [] "examples/catherineOfSienaBot.dck"

plainExpr :: (UniqueCtx, Dickinson a) -> Dickinson a
plainExpr = fst . uncurry renameDickinson

-- FIXME: StdGen in env?
txtIO :: [Declaration AlexPosn] -> IO T.Text
txtIO = fmap eitherThrow . evalIO . checkEvalM
