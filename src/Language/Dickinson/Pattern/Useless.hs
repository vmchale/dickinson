module Language.Dickinson.Pattern.Useless ( useful
                                          ) where

import           Language.Dickinson.Type

useful :: [Pattern a] -> Pattern a -> Bool
useful [] _ = True
