module Language.Dickinson.TypeCheck ( typeOf
                                    ) where

import           Control.Monad.State      (StateT)
import qualified Data.IntMap              as IM
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type

-- tyAssert :: Expression Name a -> DickinsonTy a -> Either (DickinsonError Name a) ()
-- tyAssert e ty =

type TyEnv = IM.IntMap (DickinsonTy ())

type TypeM a = StateT TyEnv (Either (DickinsonError Name a))

typeOf :: Expression Name a -> TypeM a (DickinsonTy ())
typeOf Literal{}  = pure $ TyText ()
typeOf StrChunk{} = pure $ TyText ()
