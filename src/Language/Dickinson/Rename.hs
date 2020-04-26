module Language.Dickinson.Rename ( renameDickinson
                                 ) where

import           Control.Monad.State
import qualified Data.IntMap             as IM
import qualified Data.IntSet             as IS
import qualified Data.List.NonEmpty      as NE
import           Language.Dickinson.Name
import           Language.Dickinson.Type

type Renames = (Int, IS.IntSet, IM.IntMap Int)

type RenameM a = State Renames

insertMod :: Unique -> Renames -> Renames
insertMod (Unique i) ~(m, is, rs) =
    if i `IS.member` is
        then undefined
        else (m, is, rs)

renameDickinson :: Dickinson Name a -> Dickinson Name a
renameDickinson = id

renameExpressionM :: Expression Name a -> RenameM a (Expression Name a)
renameExpressionM e@Literal{} = pure e
renameExpressionM (Choice p branches) = Choice p <$> branches'
    where branches' =
            let ds = fmap fst branches
                in let es = fmap snd branches
                    in NE.zip ds <$> traverse renameExpressionM es
