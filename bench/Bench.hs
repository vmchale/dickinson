module Main (main) where

import           Control.Exception    (throw)
import Control.Monad (void)
import           Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import           Language.Dickinson
import Data.Binary (encode)

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
                , env (void . snd <$> libParsed) $ \p ->
                  bgroup "encoder" $ 
                    [ bench "bench/data/nestLet.dck" $ nf encode p
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

plainExpr :: (UniqueCtx, Dickinson Name a) -> Dickinson Name a
plainExpr = fst . uncurry renameDickinson
