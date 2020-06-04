module Main (main) where

import           Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import           Language.Dickinson   (parse)

main :: IO ()
main =
    defaultMain [ env libFile $ \c ->
                  bgroup "parse"
                    [ bench "lib/color.dck" $ nf parse c ]
                ]

    where libFile = BSL.readFile "lib/color.dck"
