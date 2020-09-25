{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Type where

import Language.Malgo.Id
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import qualified Text.PrettyPrint.HughesPJ as P

----------------------
-- Kind and HasKind --
----------------------

data Kind = Star | KArr Kind Kind
  deriving stock (Eq, Ord, Show)

class HasKind a where
  kind :: HasCallStack => a -> Kind

instance HasKind a => HasKind (Id a) where
  kind = kind . view idMeta

instance HasKind Kind where
  kind = id

instance Pretty Kind where
  pPrintPrec _ _ Star = "*"
  pPrintPrec l d (KArr k1 k2) = P.maybeParens (d > 10) $ pPrintPrec l 11 k1 <+> "->" <+> pPrintPrec l 10 k2

----------
-- Type --
----------

data Scheme = Forall [TyVar] Type
  deriving stock (Eq, Show, Ord)

instance HasKind Scheme where
  kind (Forall _ t) = kind t

instance Pretty Scheme where
  pPrint (Forall vs t) = "forall" <+> P.sep (map pPrint vs) <> "." <+> pPrint t

-------------------
-- Type variable --
-------------------

type TyVar = Id Kind

data MetaTv = MetaTv Int Kind (IORef (Maybe Type))

instance Eq MetaTv where
  (MetaTv u1 _ _) == (MetaTv u2 _ _) = u1 == u2

instance Ord MetaTv where
  (MetaTv u1 _ _) `compare` (MetaTv u2 _ _) = u1 `compare` u2

instance Show MetaTv where
  show (MetaTv u _ _) = "_" <> show u

instance Pretty MetaTv where
  pPrint (MetaTv u _ _) = "'" <> pPrint u

instance HasKind MetaTv where
  kind (MetaTv _ k _) = k

---------------------
-- Primitive Types --
---------------------

data PrimT = Int32T | Int64T | FloatT | DoubleT | CharT | StringT
  deriving stock (Eq, Show, Ord)

instance Pretty PrimT where
  pPrint Int32T = "Int32#"
  pPrint Int64T = "Int64#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"

data Type
  = TyApp Type Type
  | TyVar TyVar
  | TyCon (Id Kind)
  | TyPrim PrimT
  | TyArr Type Type
  | TyTuple [Type]
  | TyLazy Type
  | TyMeta MetaTv
  deriving stock (Eq, Show, Ord)

makePrisms ''Type

instance HasKind Type where
  kind (TyApp t _) = case kind t of
    (KArr _ k) -> k
    _ -> error "invalid kind"
  kind (TyVar t) = kind t
  kind (TyCon c) = kind c
  kind (TyPrim _) = Star
  kind (TyArr _ _) = Star
  kind (TyTuple _) = Star
  kind (TyLazy _) = Star
  kind (TyMeta tv) = kind tv

instance Pretty Type where
  pPrintPrec l d (TyApp t1 t2) = P.maybeParens (d > 10) $ P.sep [pPrintPrec l 10 t1, pPrintPrec l 11 t2]
  pPrintPrec _ _ (TyVar v) = pPrint v
  pPrintPrec _ _ (TyCon c) = pPrint c
  pPrintPrec _ _ (TyPrim p) = pPrint p
  pPrintPrec l d (TyArr t1 t2) = P.maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TyTuple ts) = P.parens $ P.sep $ P.punctuate "," $ map pPrint ts
  pPrintPrec _ _ (TyLazy t) = P.braces $ pPrint t
  pPrintPrec _ _ (TyMeta tv) = pPrint tv

-------------------
-- HasType class --
-------------------

class HasType a where
  toType :: Optic' A_Getter NoIx a Type

instance HasType Type where
  toType = to id

instance HasType Scheme where
  toType = to $ \(Forall _ t) -> t

instance HasType a => HasType (Id a) where
  toType = idMeta % toType

data WithType a = WithType a Type
  deriving stock (Eq, Show, Ord, Functor, Foldable)

instance Pretty a => Pretty (WithType a) where
  pPrint (WithType a t) = pPrint a <> ":" <> pPrint t

instance HasType (WithType a) where
  toType = to $ \(WithType _ t) -> t

----------------
-- split Type --
----------------

splitCon :: Type -> (Id Kind, [Type])
splitCon (TyCon con) = (con, [])
splitCon (TyApp t1 t2) =
  let (dataCon, ts) = splitCon t1
   in (dataCon, ts <> [t2])
splitCon _ = bug Unreachable

splitTyArr :: Type -> ([Type], Type)
splitTyArr (TyArr t1 t2) =
  let (ps, r) = splitTyArr t2
   in (t1 : ps, r)
splitTyArr TyMeta {} = bug Unreachable
splitTyArr t = ([], t)