module Malgo.Infer.Kind (KindCtx, insertKind, askKind, HasKind (..)) where

import Control.Lens (At (at), (?~), (^.))
import Malgo.Infer.TypeRep
import Malgo.Prelude

-- * Kind context

type KindCtx = Map TypeVar Kind

insertKind :: TypeVar -> Kind -> KindCtx -> KindCtx
insertKind tv k ctx
  | k == TYPE = ctx
  | otherwise = ctx & at tv ?~ k

askKind :: TypeVar -> KindCtx -> Kind
askKind tv ctx = fromMaybe TYPE (ctx ^. at tv)

class HasKind a where
  kindOf :: KindCtx -> a -> Kind

instance HasKind TypeVar where
  kindOf ctx v = askKind v ctx

instance HasKind PrimT where
  kindOf _ _ = TYPE

instance HasKind Type where
  kindOf ctx (TyApp (kindOf ctx -> TyArr _ k) _) = k
  kindOf _ TyApp {} = error "invalid kind"
  kindOf ctx (TyVar v) = kindOf ctx v
  kindOf ctx (TyCon c) = kindOf ctx c
  kindOf ctx (TyPrim p) = kindOf ctx p
  kindOf ctx (TyArr _ t2) = kindOf ctx t2
  kindOf _ (TyTuple n) = buildTyArr (replicate n TYPE) TYPE
  kindOf _ (TyRecord _) = TYPE
  kindOf _ TyPtr = TYPE `TyArr` TYPE
  kindOf _ TYPE = TYPE -- Type :: Type
  kindOf ctx (TyMeta tv) = kindOf ctx tv

instance HasKind Void where
  kindOf _ = absurd

instance HasKind MetaVar where
  kindOf ctx MetaVar {metaVar} = askKind metaVar ctx

instance (HasKind ty) => HasKind (TypeDef ty) where
  kindOf ctx TypeDef {typeConstructor} = kindOf ctx typeConstructor
