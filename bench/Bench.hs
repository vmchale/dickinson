module Main (main) where

import           Control.Exception    (throw)
import           Control.Monad        (void)
import           Criterion.Main
import           Data.Binary          (decode, encode)
import qualified Data.ByteString.Lazy as BSL
import           Language.Dickinson

main :: IO ()
main =
    defaultMain [ env libFile $ \c ->
                  bgroup "lib/color.dck"
                    [ bench "parse" $ nf parse c
                    , bench "lex" $ nf lexDickinson c
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
          libParsed = either throw id . parseWithCtx <$> BSL.readFile "bench/data/nestLet.dck"
          multiParsed = either throw id . parse <$> BSL.readFile "bench/data/multiple.dck"
          encoded = encode . map void <$> multiParsed

plainExpr :: (UniqueCtx, Dickinson Name a) -> Dickinson Name a
plainExpr = fst . uncurry renameDickinson
