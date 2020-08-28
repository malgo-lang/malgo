{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Syntax where

import Data.Int (Int32, Int64)
import Language.Griff.Extension
import Language.Griff.Type (HasType (..))
import qualified Language.Griff.Type as T
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import qualified Text.PrettyPrint.HughesPJ as P

-- Unboxed literal

data Unboxed = Bool Bool | Int32 Int32 | Int64 Int64 | Float Float | Double Double | Char Char | String String
  deriving stock (Show, Eq, Ord)

instance Pretty Unboxed where
  pPrint (Bool b) = if b then "True#" else "False#"
  pPrint (Int32 i) = P.int (fromIntegral i) <> "#"
  pPrint (Int64 i) = P.int (fromIntegral i) <> "L#"
  pPrint (Float f) = P.float f <> "F#"
  pPrint (Double d) = P.double d <> "#"
  pPrint (Char c) = P.quotes (P.char c) <> "#"
  pPrint (String s) = P.doubleQuotes (P.text s) <> "#"

-- Expression

data Exp x
  = Var (XVar x) (XId x)
  | Con (XCon x) (XId x)
  | Unboxed (XUnboxed x) Unboxed
  | Apply (XApply x) (Exp x) (Exp x)
  | OpApp (XOpApp x) (XId x) (Exp x) (Exp x)
  | Fn (XFn x) [Clause x]
  | Tuple (XTuple x) [Exp x]
  | Force (XForce x) (Exp x)

deriving stock instance (ForallExpX Eq x, ForallClauseX Eq x, ForallPatX Eq x, Eq (XId x)) => Eq (Exp x)

deriving stock instance (ForallExpX Show x, ForallClauseX Show x, ForallPatX Show x, Show (XId x)) => Show (Exp x)

instance (Pretty (XId x)) => Pretty (Exp x) where
  pPrintPrec _ _ (Var _ i) = pPrint i
  pPrintPrec _ _ (Con _ c) = pPrint c
  pPrintPrec _ _ (Unboxed _ u) = pPrint u
  pPrintPrec l d (Apply _ e1 e2) = P.maybeParens (d > 10) $ P.sep [pPrintPrec l 10 e1, pPrintPrec l 11 e2]
  pPrintPrec l d (OpApp _ o e1 e2) =
    P.maybeParens (d > 10) $
      P.sep [pPrintPrec l 11 e1, pPrintPrec l 10 o, pPrintPrec l 11 e2]
  pPrintPrec l _ (Fn _ cs) =
    P.braces $
      P.space
        <> foldl1
          (\a b -> P.sep [a, P.nest (-2) $ "|" <+> b])
          (map (pPrintPrec l 0) cs)
  pPrintPrec _ _ (Tuple _ xs) = P.parens $ P.sep $ P.punctuate "," $ map pPrint xs
  pPrintPrec l _ (Force _ x) = pPrintPrec l 11 x <> "!"

instance (ForallExpX HasType x, ForallClauseX HasType x, ForallPatX HasType x) => HasType (Exp x) where
  typeOf = lens getter setter
    where
      getter (Var x _) = view typeOf x
      getter (Con x _) = view typeOf x
      getter (Unboxed x _) = view typeOf x
      getter (Apply x _ _) = view typeOf x
      getter (OpApp x _ _ _) = view typeOf x
      getter (Fn x _) = view typeOf x
      getter (Tuple x _) = view typeOf x
      getter (Force x _) = view typeOf x
      setter (Var x v) t = Var (set typeOf t x) v
      setter (Con x c) t = Con (set typeOf t x) c
      setter (Unboxed x u) t
        | view typeOf x == t = Unboxed x u
        | otherwise = errorDoc $ "Panic!" <+> "typeOf" <+> P.parens (pPrint u) <+> "is not" <+> pPrint t
      setter (Apply x e1 e2) t = Apply (set typeOf t x) (set typeOf t e1) (set typeOf t e2)
      setter (OpApp x op e1 e2) t = OpApp (set typeOf t x) op (set typeOf t e1) (set typeOf t e2)
      setter (Fn x cs) t = Fn (set typeOf t x) (map (set typeOf t) cs)
      setter (Tuple x es) (T.TyTuple ts) = Tuple (set typeOf (T.TyTuple ts) x) (zipWith (set typeOf) ts es)
      setter Tuple {} t = errorDoc $ "Panic!" <+> pPrint t <+> "is not a tuple"
      setter (Force x e) t = Force (set typeOf t x) (set typeOf t e)

------------
-- Clause --
------------

data Clause x = Clause (XClause x) [Pat x] (Exp x)

deriving stock instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x, Eq (XId x)) => Eq (Clause x)

deriving stock instance (ForallClauseX Show x, ForallExpX Show x, ForallPatX Show x, Show (XId x)) => Show (Clause x)

instance (Pretty (XId x)) => Pretty (Clause x) where
  pPrint (Clause _ pats e) = P.sep (map pPrint pats) <+> "->" <+> pPrint e

instance (ForallExpX HasType x, ForallClauseX HasType x, ForallPatX HasType x) => HasType (Clause x) where
  typeOf = lens getter setter
    where
      getter (Clause x _ _) = view typeOf x
      setter (Clause x ps e) t = Clause (set typeOf t x) (zipWith (set typeOf) pts ps) (set typeOf et e)
        where
          (pts, et) = splitTyArr ps t
          splitTyArr (_ : ps') (T.TyArr t1 t2) =
            let (pts', et') = splitTyArr ps' t2
             in (t1 : pts', et')
          splitTyArr [] t = ([], t)
          splitTyArr _ t = ([], t)

-------------
-- Pattern --
-------------

data Pat x
  = VarP (XVarP x) (XId x)
  | ConP (XConP x) (XId x) [Pat x]
  | UnboxedP (XUnboxedP x) Unboxed

deriving stock instance (ForallPatX Eq x, Eq (XId x)) => Eq (Pat x)

deriving stock instance (ForallPatX Show x, Show (XId x)) => Show (Pat x)

instance (Pretty (XId x)) => Pretty (Pat x) where
  pPrintPrec _ _ (VarP _ i) = pPrint i
  pPrintPrec _ _ (ConP _ i []) = pPrint i
  pPrintPrec l d (ConP _ i ps) = P.maybeParens (d > 10) $ pPrint i <+> P.sep (map (pPrintPrec l 11) ps)
  pPrintPrec _ _ (UnboxedP _ u) = pPrint u

instance (ForallPatX HasType x) => HasType (Pat x) where
  typeOf = lens getter setter
    where
      getter (VarP x _) = view typeOf x
      getter (ConP x _ _) = view typeOf x
      getter (UnboxedP x _) = view typeOf x
      setter (VarP x v) t = VarP (set typeOf t x) v
      setter (ConP x c ps) t = ConP (set typeOf t x) c (zipWith (set typeOf) pts ps)
        where
          pts = go t []
          go (T.TyApp t1 t2) acc = go t1 (t2 : acc)
          go _ acc = acc
      setter (UnboxedP x u) t
        | view typeOf x == t = UnboxedP x u
        | otherwise = errorDoc $ "Panic!" <+> "typeOf" <+> P.parens (pPrint u) <+> "is not" <+> pPrint t

----------
-- Type --
----------

data Type x
  = TyApp (XTyApp x) (Type x) [Type x]
  | TyVar (XTyVar x) (XTId x)
  | TyCon (XTyCon x) (XTId x)
  | TyArr (XTyArr x) (Type x) (Type x)
  | TyTuple (XTyTuple x) [Type x]
  | TyLazy (XTyLazy x) (Type x)

deriving stock instance (ForallTypeX Eq x, Eq (XTId x)) => Eq (Type x)

deriving stock instance (ForallTypeX Show x, Show (XTId x)) => Show (Type x)

instance (Pretty (XTId x)) => Pretty (Type x) where
  pPrintPrec l d (TyApp _ t ts) = P.maybeParens (d > 11) $ pPrint t <+> P.sep (map (pPrintPrec l 12) ts)
  pPrintPrec _ _ (TyVar _ i) = pPrint i
  pPrintPrec _ _ (TyCon _ i) = pPrint i
  pPrintPrec l d (TyArr _ t1 t2) = P.maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TyTuple _ ts) = P.parens $ P.sep $ P.punctuate "," $ map pPrint ts
  pPrintPrec _ _ (TyLazy _ t) = P.braces $ pPrint t

-----------------
-- Declaration --
-----------------

data Decl x
  = ScDef (XScDef x) (XId x) [XId x] (Exp x)
  | ScSig (XScSig x) (XId x) (Type x)
  | DataDef (XDataDef x) (XTId x) [XTId x] [(XId x, [Type x])]
  | Infix (XInfix x) Assoc Int (XId x)
  | Forign (XForign x) (XId x) (Type x)

deriving stock instance (ForallDeclX Eq x, Eq (XId x), Eq (XTId x)) => Eq (Decl x)

deriving stock instance (ForallDeclX Show x, Show (XId x), Show (XTId x)) => Show (Decl x)

instance (Pretty (XId x), Pretty (XTId x)) => Pretty (Decl x) where
  pPrint (ScDef _ f xs e) = P.sep [pPrint f <+> P.sep (map pPrint xs) <+> "=", P.nest 2 $ pPrint e]
  pPrint (ScSig _ f t) = pPrint f <+> "::" <+> pPrint t
  pPrint (DataDef _ d xs cs) = P.sep ["data" <+> pPrint d <+> P.sep (map pPrint xs) <+> "=", foldl1 (\a b -> P.sep [a, "|" <+> b]) $ map pprConDef cs]
    where
      pprConDef (con, ts) = pPrint con <+> P.sep (map pPrint ts)
  pPrint (Infix _ a o x) = "infix" <> pPrint a <+> pPrint o <+> pPrint x
  pPrint (Forign _ x t) = "forign import" <+> pPrint x <+> "::" <+> pPrint t