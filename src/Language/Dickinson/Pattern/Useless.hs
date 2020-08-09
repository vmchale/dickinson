-- see Maranget, Warnings for Pattern Matching
-- https://www.cambridge.org/core/journals/journal-of-functional-programming/article/warnings-for-pattern-matching/3165B75113781E2431E3856972940347
module Language.Dickinson.Pattern.Useless ( useful
                                          ) where

-- TODO: unboxed?
import           Data.Array              (bounds)
import qualified Data.Array              as A
import           Data.Foldable           (toList)
import           Data.List.NonEmpty      (NonEmpty)
import           Language.Dickinson.Type

wildcardVec :: a -> A.Array Int (Pattern a)
wildcardVec loc = A.listArray (1,1) [Wildcard loc]

matchToMatrix :: NonEmpty a -> A.Array (Int, Int) a
matchToMatrix ps = A.listArray dim (toList ps)
    where sz = length ps
          dim = ((1,1), (sz,1))

numRows :: A.Array (Int, Int) a -> Int
numRows arr =
    let ((_,_), (rows,_)) = bounds arr
        in rows

numColumns :: A.Array (Int, Int) a -> Int
numColumns arr =
    let ((_,_), (_,cols)) = bounds arr
        in cols

useful :: A.Array (Int, Int) (Pattern a) -> A.Array Int (Pattern a) -> Bool
useful arr _ | numRows arr == 0 = True
             | numColumns arr == 0 = False -- b/c numRows > 0
useful arr q =
    case q A.! 0 of -- safe because n > 0, i.e. there are columns
        _ -> undefined -- TODO: could use list for q

-- specialized :: A.Array (Int, Int) (Pattern a) -> A.Array Int (Pattern a) -> A.Array (Int, Int) (Pattern a)
-- specialized _ _ = undefined
