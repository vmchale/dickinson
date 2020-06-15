module Main (main) where

import           Control.Exception    (throw)
import           Control.Monad        (void)
import           Criterion.Main
import           Data.Binary          (decode, encode)
import qualified Data.ByteString.Lazy as BSL
import           Language.Dickinson

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
                , env (void <$> multiParsed) $ \p ->
                  bgroup "encoder"
                    [ bench "bench/data/multiple.dck" $ nf encode p
                    ]
                , env encoded $ \e ->
                  bgroup "decoder"
                    [ bench "bench/data/multiple.dck" $ nf (decode :: BSL.ByteString -> Dickinson Name ()) e
                    ]
                , env multiParsed $ \p ->
                  bgroup "multiple"
                    [ bench "bench/data/multiple.dck" $ nf checkMultiple p
                    ]
                , bgroup "result"
                    [ bench "test/eval/context.dck" $ nfIO (evalFile "test/eval/context.dck")
                    ]
                ]

    where libFile = BSL.readFile "lib/color.dck"
          shakespeare = BSL.readFile "examples/shakespeare.dck"
          parses = (,) <$> libFile <*> shakespeare
          libParsed = either throw id . parseWithCtx <$> BSL.readFile "bench/data/nestLet.dck"
          multiParsed = either throw id . parse <$> BSL.readFile "bench/data/multiple.dck"
          encoded = encode . yeet <$> multiParsed

yeet :: Dickinson Name AlexPosn -> Dickinson Name ()
yeet = fmap void

plainExpr :: (UniqueCtx, Dickinson Name a) -> Dickinson Name a
plainExpr = fst . uncurry renameDickinson
