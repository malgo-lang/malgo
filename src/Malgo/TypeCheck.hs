module Malgo.TypeCheck (typeCheck) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Malgo.Monad
import Malgo.Prelude
import Malgo.Syntax

type TypeError = Text

type TypeCtx = Map Id Type

data Type
  = -- | type variable, e.g. @a@ in @id : a -> a@
    VarTy Id
  | IntTy
  | FunTy [Type] Type
  | -- | meta variable used in unification
    MetaTy Id
  deriving stock (Eq, Ord, Show, Generic)

typeCheck :: Expr Id -> MalgoM (Either TypeError TypeCtx)
typeCheck = undefined