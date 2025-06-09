module Malgo.Infer.Kind (KindCtx, insertKind, askKind, HasKind (..)) where

import Data.Map.Strict qualified as Map
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Malgo.Infer.Error
import Malgo.Infer.TypeRep
import Malgo.Prelude

-- * Kind context

type KindCtx = Map TypeVar Kind

insertKind :: TypeVar -> Kind -> KindCtx -> KindCtx
insertKind tv k ctx
  | k == TYPE = ctx
  | otherwise = Map.insert tv k ctx

askKind :: TypeVar -> KindCtx -> Kind
askKind tv ctx = fromMaybe TYPE (Map.lookup tv ctx)

class HasKind a where
  kindOf :: (Error InferError :> es) => Range -> KindCtx -> a -> Eff es Kind

instance HasKind TypeVar where
  kindOf _ ctx v = pure $ askKind v ctx

instance HasKind PrimT where
  kindOf _ _ _ = pure TYPE

instance HasKind Type where
  kindOf range ctx (TyApp t1 t2) = do
    kind <- kindOf range ctx t1
    case kind of
      TyArr _ k -> pure k
      _ -> throwError $ InvalidTypeApplication range t1 t2
  kindOf range ctx (TyVar v) = kindOf range ctx v
  kindOf range ctx (TyCon c) = kindOf range ctx c
  kindOf range ctx (TyPrim p) = kindOf range ctx p
  kindOf range ctx (TyArr _ t2) = kindOf range ctx t2
  kindOf _ _ (TyTuple n) = pure $ buildTyArr (replicate n TYPE) TYPE
  kindOf _ _ (TyRecord _) = pure TYPE
  kindOf _ _ TyPtr = pure $ TYPE `TyArr` TYPE
  kindOf _ _ TYPE = pure TYPE -- Type :: Type
  kindOf range ctx (TyMeta tv) = kindOf range ctx tv

instance HasKind Void where
  kindOf _ _ = absurd

instance HasKind MetaVar where
  kindOf _ ctx MetaVar {metaVar} = pure $ askKind metaVar ctx

instance (HasKind ty) => HasKind (TypeDef ty) where
  kindOf range ctx TypeDef {typeConstructor} = kindOf range ctx typeConstructor
