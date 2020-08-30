{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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
  pPrint (MetaTv u _ _) = "$_" <> pPrint u

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

-------------------
-- HasType class --
-------------------

class HasType a where
  typeOf :: Lens' a Type

instance HasType Type where
  typeOf = id

instance HasType Scheme where
  typeOf f (Forall vs t) = Forall vs <$> typeOf f t

instance HasType a => HasType (Id a) where
  typeOf f = idMeta (typeOf f)

data WithType a = WithType a Type
  deriving stock (Eq, Show, Ord, Functor, Foldable)

instance HasType (WithType a) where
  typeOf f (WithType a t) = WithType a <$> typeOf f t