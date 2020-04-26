module Language.Dickinson.Rename ( renameDickinson
                                 ) where

import           Control.Monad.State
import qualified Data.IntMap             as IM
import qualified Data.IntSet             as IS
import qualified Data.List.NonEmpty      as NE
import           Language.Dickinson.Name
import           Language.Dickinson.Type

type Renames = IM.IntMap Int

type RenameM a = State (IS.IntSet, Renames)

renameDickinson :: Dickinson Name a -> Dickinson Name a
renameDickinson = id

renameExpression :: Expression Name a -> RenameM a (Expression Name a)
renameExpression e@Literal{} = pure e
renameExpression (Choice p branches) = Choice p <$> branches'
    where branches' =
            let ds = fmap fst branches
                in let es = fmap snd branches
                    in NE.zip ds <$> traverse renameExpression es
