{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Syntax where

import Data.Functor (($>))
import Data.Graph (flattenSCC, stronglyConnComp)
import Data.Int (Int32, Int64)
import qualified Data.Set as Set
import Data.Tuple.Extra (uncurry3)
import Koriel.Id
import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.Syntax.Extension
import qualified Language.Malgo.TypeRep.IORef as I
import qualified Language.Malgo.TypeRep.Static as S
import qualified Language.Malgo.TypeRep.UTerm as U
import qualified Language.Malgo.Unify as U

-- | Unboxed and literal
data Literal x = Int32 Int32 | Int64 Int64 | Float Float | Double Double | Char Char | String String
  deriving stock (Show, Eq, Ord)

instance Pretty (Literal x) where
  pPrint (Int32 i) = pPrint (toInteger i)
  pPrint (Int64 i) = pPrint (toInteger i) <> "L"
  pPrint (Float f) = pPrint f <> "F"
  pPrint (Double d) = pPrint d
  pPrint (Char c) = quotes (pPrint c)
  pPrint (String s) = doubleQuotes (text s)

instance I.HasType (Literal x) where
  toType = to $ \case
    Int32 {} -> I.TyPrim S.Int32T
    Int64 {} -> I.TyPrim S.Int64T
    Float {} -> I.TyPrim S.FloatT
    Double {} -> I.TyPrim S.DoubleT
    Char {} -> I.TyPrim S.CharT
    String {} -> I.TyPrim S.StringT
  overType _ = pure

instance U.HasType U.UType (Literal x) where
  typeOf Int32 {} = U.UTerm (U.TyPrim S.Int32T)
  typeOf Int64 {} = U.UTerm (U.TyPrim S.Int64T)
  typeOf Float {} = U.UTerm (U.TyPrim S.FloatT)
  typeOf Double {} = U.UTerm (U.TyPrim S.DoubleT)
  typeOf Char {} = U.UTerm (U.TyPrim S.CharT)
  typeOf String {} = U.UTerm (U.TyPrim S.StringT)

instance S.HasType (Literal x) where
  typeOf Int32 {} = S.TyPrim S.Int32T
  typeOf Int64 {} = S.TyPrim S.Int64T
  typeOf Float {} = S.TyPrim S.FloatT
  typeOf Double {} = S.TyPrim S.DoubleT
  typeOf Char {} = S.TyPrim S.CharT
  typeOf String {} = S.TyPrim S.StringT

instance U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind) (Literal x) where
  walkOn f v = f (U.typeOf v) $> v

toUnboxed :: Literal Boxed -> Literal Unboxed
toUnboxed = coerce

-- Expression

data Exp x
  = Var (XVar x) (XId x)
  | Con (XCon x) (XId x)
  | Unboxed (XUnboxed x) (Literal Unboxed)
  | Boxed (XBoxed x) (Literal Boxed)
  | Apply (XApply x) (Exp x) (Exp x)
  | OpApp (XOpApp x) (XId x) (Exp x) (Exp x)
  | Fn (XFn x) [Clause x]
  | Tuple (XTuple x) [Exp x]
  | Force (XForce x) (Exp x)
  | Parens (XParens x) (Exp x)

deriving stock instance (ForallExpX Eq x, ForallClauseX Eq x, ForallPatX Eq x, ForallStmtX Eq x, Eq (XId x)) => Eq (Exp x)

deriving stock instance (ForallExpX Show x, ForallClauseX Show x, ForallPatX Show x, ForallStmtX Show x, Show (XId x)) => Show (Exp x)

instance (Pretty (XId x)) => Pretty (Exp x) where
  pPrintPrec _ _ (Var _ i) = pPrint i
  pPrintPrec _ _ (Con _ c) = pPrint c
  pPrintPrec _ _ (Unboxed _ lit) = pPrint lit <> "#"
  pPrintPrec _ _ (Boxed _ lit) = pPrint lit
  pPrintPrec l d (Apply _ e1 e2) =
    maybeParens (d > 10) $ sep [pPrintPrec l 10 e1, pPrintPrec l 11 e2]
  pPrintPrec l d (OpApp _ o e1 e2) =
    maybeParens (d > 10) $ sep [pPrintPrec l 11 e1, pPrintPrec l 10 o <+> pPrintPrec l 11 e2]
  pPrintPrec l _ (Fn _ cs) =
    braces $
      space
        <> foldl1
          (\a b -> sep [a, nest (-2) $ "|" <+> b])
          (map (pPrintPrec l 0) cs)
  pPrintPrec _ _ (Tuple _ xs) = parens $ sep $ punctuate "," $ map pPrint xs
  pPrintPrec l _ (Force _ x) = "!" <> pPrintPrec l 11 x
  pPrintPrec _ _ (Parens _ x) = parens $ pPrint x

instance (ForallExpX I.HasType x, ForallClauseX I.HasType x, ForallPatX I.HasType x) => I.HasType (Exp x) where
  toType = to $ \case
    Var x _ -> x ^. I.toType
    Con x _ -> x ^. I.toType
    Unboxed x _ -> x ^. I.toType
    Boxed x _ -> x ^. I.toType
    Apply x _ _ -> x ^. I.toType
    OpApp x _ _ _ -> x ^. I.toType
    Fn x _ -> x ^. I.toType
    Tuple x _ -> x ^. I.toType
    Force x _ -> x ^. I.toType
    Parens x _ -> x ^. I.toType
  overType f = \case
    Var x v -> Var <$> I.overType f x <*> pure v
    Con x c -> Con <$> I.overType f x <*> pure c
    Unboxed x u -> Unboxed <$> I.overType f x <*> I.overType f u
    Boxed x b -> Boxed <$> I.overType f x <*> I.overType f b
    Apply x e1 e2 -> Apply <$> I.overType f x <*> I.overType f e1 <*> I.overType f e2
    OpApp x op e1 e2 -> OpApp <$> I.overType f x <*> pure op <*> I.overType f e1 <*> I.overType f e2
    Fn x cs -> Fn <$> I.overType f x <*> traverse (I.overType f) cs
    Tuple x es -> Tuple <$> I.overType f x <*> traverse (I.overType f) es
    Force x e -> Force <$> I.overType f x <*> I.overType f e
    Parens x e -> Parens <$> I.overType f x <*> I.overType f e

instance
  ( ForallExpX (U.HasType U.UType) x,
    ForallClauseX (U.HasType U.UType) x,
    ForallPatX (U.HasType U.UType) x
  ) =>
  U.HasType U.UType (Exp x)
  where
  typeOf (Var x _) = U.typeOf x
  typeOf (Con x _) = U.typeOf x
  typeOf (Unboxed x _) = U.typeOf x
  typeOf (Boxed x _) = U.typeOf x
  typeOf (Apply x _ _) = U.typeOf x
  typeOf (OpApp x _ _ _) = U.typeOf x
  typeOf (Fn x _) = U.typeOf x
  typeOf (Tuple x _) = U.typeOf x
  typeOf (Force x _) = U.typeOf x
  typeOf (Parens x _) = U.typeOf x

instance
  ( ForallExpX S.HasType x,
    ForallClauseX S.HasType x,
    ForallPatX S.HasType x
  ) =>
  S.HasType (Exp x)
  where
  typeOf (Var x _) = S.typeOf x
  typeOf (Con x _) = S.typeOf x
  typeOf (Unboxed x _) = S.typeOf x
  typeOf (Boxed x _) = S.typeOf x
  typeOf (Apply x _ _) = S.typeOf x
  typeOf (OpApp x _ _ _) = S.typeOf x
  typeOf (Fn x _) = S.typeOf x
  typeOf (Tuple x _) = S.typeOf x
  typeOf (Force x _) = S.typeOf x
  typeOf (Parens x _) = S.typeOf x

instance
  ( ForallExpX (U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind)) x,
    ForallClauseX (U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind)) x,
    ForallPatX (U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind)) x
  ) =>
  U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind) (Exp x)
  where
  walkOn f = \case
    Var x v -> Var <$> U.walkOn f x <*> pure v
    Con x c -> Con <$> U.walkOn f x <*> pure c
    Unboxed x u -> Unboxed <$> U.walkOn f x <*> U.walkOn f u
    Boxed x b -> Boxed <$> U.walkOn f x <*> U.walkOn f b
    Apply x e1 e2 -> Apply <$> U.walkOn f x <*> U.walkOn f e1 <*> U.walkOn f e2
    OpApp x op e1 e2 -> OpApp <$> U.walkOn f x <*> pure op <*> U.walkOn f e1 <*> U.walkOn f e2
    Fn x cs -> Fn <$> U.walkOn f x <*> traverse (U.walkOn f) cs
    Tuple x es -> Tuple <$> U.walkOn f x <*> traverse (U.walkOn f) es
    Force x e -> Force <$> U.walkOn f x <*> U.walkOn f e
    Parens x e -> Parens <$> U.walkOn f x <*> U.walkOn f e

freevars :: Ord (XId x) => Exp x -> Set (XId x)
freevars (Var _ v) = Set.singleton v
freevars (Con _ _) = Set.empty
freevars (Unboxed _ _) = Set.empty
freevars (Boxed _ _) = Set.empty
freevars (Apply _ e1 e2) = freevars e1 <> freevars e2
freevars (OpApp _ op e1 e2) = Set.insert op $ freevars e1 <> freevars e2
freevars (Fn _ cs) = mconcat $ map freevarsClause cs
freevars (Tuple _ es) = mconcat $ map freevars es
freevars (Force _ e) = freevars e
freevars (Parens _ e) = freevars e

------------
-- Clause --
------------

data Stmt x
  = Let (XLet x) (XId x) (Exp x)
  | NoBind (XNoBind x) (Exp x)

deriving stock instance (ForallClauseX Eq x, ForallPatX Eq x, ForallExpX Eq x, ForallStmtX Eq x, Eq (XId x)) => Eq (Stmt x)

deriving stock instance (ForallClauseX Show x, ForallPatX Show x, ForallExpX Show x, ForallStmtX Show x, Show (XId x)) => Show (Stmt x)

instance Pretty (XId x) => Pretty (Stmt x) where
  pPrint (Let _ v e) = "let" <+> pPrint v <+> "=" <+> pPrint e
  pPrint (NoBind _ e) = pPrint e

instance (ForallExpX I.HasType x, ForallClauseX I.HasType x, ForallPatX I.HasType x) => I.HasType (Stmt x) where
  toType = to $ \case
    Let _ _ e -> view I.toType e
    NoBind _ e -> view I.toType e
  overType f = \case
    Let x v e -> Let x v <$> I.overType f e
    NoBind x e -> NoBind x <$> I.overType f e

instance
  ( ForallExpX (U.HasType U.UType) x,
    ForallClauseX (U.HasType U.UType) x,
    ForallPatX (U.HasType U.UType) x
  ) =>
  U.HasType U.UType (Stmt x)
  where
  typeOf (Let _ _ e) = U.typeOf e
  typeOf (NoBind _ e) = U.typeOf e

instance
  ( ForallExpX S.HasType x,
    ForallClauseX S.HasType x,
    ForallPatX S.HasType x
  ) =>
  S.HasType (Stmt x)
  where
  typeOf (Let _ _ e) = S.typeOf e
  typeOf (NoBind _ e) = S.typeOf e

instance
  ( ForallExpX (U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind)) x,
    ForallClauseX (U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind)) x,
    ForallPatX (U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind)) x
  ) =>
  U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind) (Stmt x)
  where
  walkOn f = \case
    Let x v e -> Let x v <$> U.walkOn f e
    NoBind x e -> NoBind x <$> U.walkOn f e

freevarsStmt :: Ord (XId x) => Stmt x -> Set (XId x)
freevarsStmt (Let _ x e) = Set.delete x $ freevars e
freevarsStmt (NoBind _ e) = freevars e

-- [Exp x]は、末尾へのアクセスが速いものに変えたほうが良いかも
data Clause x = Clause (XClause x) [Pat x] [Stmt x]

deriving stock instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x, ForallStmtX Eq x, Eq (XId x)) => Eq (Clause x)

deriving stock instance (ForallClauseX Show x, ForallExpX Show x, ForallPatX Show x, ForallStmtX Show x, Show (XId x)) => Show (Clause x)

instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x, Ord (XId x), ForallPatX Ord x, ForallStmtX Ord x) => Ord (Clause x) where
  (Clause _ ps1 _) `compare` (Clause _ ps2 _) = ps1 `compare` ps2

instance (Pretty (XId x)) => Pretty (Clause x) where
  pPrintPrec _ _ (Clause _ [] e) = sep (punctuate ";" $ map pPrint e)
  pPrintPrec l _ (Clause _ ps e) = sep [sep (map (pPrintPrec l 11) ps) <+> "->", sep (punctuate ";" $ map pPrint e)]

instance (ForallExpX I.HasType x, ForallClauseX I.HasType x, ForallPatX I.HasType x) => I.HasType (Clause x) where
  toType = to $ \(Clause x _ _) -> view I.toType x
  overType f (Clause x ps e) =
    Clause <$> I.overType f x <*> traverse (I.overType f) ps <*> traverse (I.overType f) e

instance
  ( ForallExpX (U.HasType U.UType) x,
    ForallClauseX (U.HasType U.UType) x,
    ForallPatX (U.HasType U.UType) x
  ) =>
  U.HasType U.UType (Clause x)
  where
  typeOf (Clause x _ _) = U.typeOf x

instance
  ( ForallExpX S.HasType x,
    ForallClauseX S.HasType x,
    ForallPatX S.HasType x
  ) =>
  S.HasType (Clause x)
  where
  typeOf (Clause x _ _) = S.typeOf x

instance
  ( ForallExpX (U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind)) x,
    ForallClauseX (U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind)) x,
    ForallPatX (U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind)) x
  ) =>
  U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind) (Clause x)
  where
  walkOn f (Clause x ps e) = Clause <$> U.walkOn f x <*> traverse (U.walkOn f) ps <*> traverse (U.walkOn f) e

freevarsClause :: Ord (XId x) => Clause x -> Set (XId x)
freevarsClause (Clause _ pats es) = foldMap freevarsStmt es Set.\\ mconcat (map bindVars pats)

-------------
-- Pattern --
-------------

data Pat x
  = VarP (XVarP x) (XId x)
  | ConP (XConP x) (XId x) [Pat x]
  | TupleP (XTupleP x) [Pat x]
  | UnboxedP (XUnboxedP x) (Literal Unboxed)

deriving stock instance (ForallPatX Eq x, Eq (XId x)) => Eq (Pat x)

deriving stock instance (ForallPatX Show x, Show (XId x)) => Show (Pat x)

deriving stock instance (ForallPatX Ord x, Ord (XId x)) => Ord (Pat x)

instance (Pretty (XId x)) => Pretty (Pat x) where
  pPrintPrec _ _ (VarP _ i) = pPrint i
  pPrintPrec _ _ (ConP _ i []) = pPrint i
  pPrintPrec l d (ConP _ i ps) =
    maybeParens (d > 10) $ pPrint i <+> sep (map (pPrintPrec l 11) ps)
  pPrintPrec _ _ (TupleP _ ps) =
    parens $ sep $ punctuate "," $ map pPrint ps
  pPrintPrec _ _ (UnboxedP _ u) = pPrint u

instance (ForallPatX I.HasType x) => I.HasType (Pat x) where
  toType = to $ \case
    VarP x _ -> view I.toType x
    ConP x _ _ -> view I.toType x
    TupleP x _ -> view I.toType x
    UnboxedP x _ -> view I.toType x
  overType f = \case
    VarP x v -> VarP <$> I.overType f x <*> pure v
    ConP x c ps -> ConP <$> I.overType f x <*> pure c <*> traverse (I.overType f) ps
    TupleP x ps -> TupleP <$> I.overType f x <*> traverse (I.overType f) ps
    UnboxedP x u -> UnboxedP <$> I.overType f x <*> I.overType f u

instance
  ( ForallExpX (U.HasType U.UType) x,
    ForallClauseX (U.HasType U.UType) x,
    ForallPatX (U.HasType U.UType) x
  ) =>
  U.HasType U.UType (Pat x)
  where
  typeOf (VarP x _) = U.typeOf x
  typeOf (ConP x _ _) = U.typeOf x
  typeOf (TupleP x _) = U.typeOf x
  typeOf (UnboxedP x _) = U.typeOf x

instance
  ( ForallExpX S.HasType x,
    ForallClauseX S.HasType x,
    ForallPatX S.HasType x
  ) =>
  S.HasType (Pat x)
  where
  typeOf (VarP x _) = S.typeOf x
  typeOf (ConP x _ _) = S.typeOf x
  typeOf (TupleP x _) = S.typeOf x
  typeOf (UnboxedP x _) = S.typeOf x

instance
  ( ForallExpX (U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind)) x,
    ForallClauseX (U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind)) x,
    ForallPatX (U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind)) x
  ) =>
  U.HasUTerm (U.TypeF U.UKind) (U.TypeVar U.UKind) (Pat x)
  where
  walkOn f = \case
    VarP x v -> VarP <$> U.walkOn f x <*> pure v
    ConP x c ps -> ConP <$> U.walkOn f x <*> pure c <*> traverse (U.walkOn f) ps
    TupleP x ps -> TupleP <$> U.walkOn f x <*> traverse (U.walkOn f) ps
    UnboxedP x u -> UnboxedP <$> U.walkOn f x <*> U.walkOn f u

_VarP :: Prism' (Pat x) (XVarP x, XId x)
_VarP = prism' (uncurry VarP) $ \case
  VarP x v -> Just (x, v)
  _ -> Nothing

_ConP :: Prism' (Pat x) (XConP x, XId x, [Pat x])
_ConP = prism' (uncurry3 ConP) $ \case
  ConP x c ps -> Just (x, c, ps)
  _ -> Nothing

_TupleP :: Prism' (Pat x) (XTupleP x, [Pat x])
_TupleP = prism' (uncurry TupleP) $ \case
  TupleP x ps -> Just (x, ps)
  _ -> Nothing

_UnboxedP :: Prism' (Pat x) (XUnboxedP x, Literal Unboxed)
_UnboxedP = prism' (uncurry UnboxedP) $ \case
  UnboxedP x u -> Just (x, u)
  _ -> Nothing

bindVars :: Ord (XId x) => Pat x -> Set (XId x)
bindVars (VarP _ x) = Set.singleton x
bindVars (ConP _ _ ps) = mconcat $ map bindVars ps
bindVars (TupleP _ ps) = mconcat $ map bindVars ps
bindVars UnboxedP {} = Set.empty

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
  pPrintPrec l d (TyApp _ t ts) =
    maybeParens (d > 11) $ pPrint t <+> sep (map (pPrintPrec l 12) ts)
  pPrintPrec _ _ (TyVar _ i) = pPrint i
  pPrintPrec _ _ (TyCon _ i) = pPrint i
  pPrintPrec l d (TyArr _ t1 t2) =
    maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TyTuple _ ts) = parens $ sep $ punctuate "," $ map pPrint ts
  pPrintPrec _ _ (TyLazy _ t) = braces $ pPrint t

getTyVars :: Ord (XTId x) => Type x -> Set (XTId x)
getTyVars (TyApp _ t ts) = getTyVars t <> mconcat (map getTyVars ts)
getTyVars (TyVar _ v) = Set.singleton v
getTyVars TyCon {} = mempty
getTyVars (TyArr _ t1 t2) = getTyVars t1 <> getTyVars t2
getTyVars (TyTuple _ ts) = mconcat $ map getTyVars ts
getTyVars (TyLazy _ t) = getTyVars t

-----------------
-- Declaration --
-----------------

data Decl x
  = ScDef (XScDef x) (XId x) (Exp x)
  | ScSig (XScSig x) (XId x) (Type x)
  | DataDef (XDataDef x) (XTId x) [XTId x] [(XId x, [Type x])]
  | Infix (XInfix x) Assoc Int (XId x)
  | Foreign (XForeign x) (XId x) (Type x)
  | Import (XImport x) ModuleName

deriving stock instance (ForallDeclX Eq x, Eq (XId x), Eq (XTId x)) => Eq (Decl x)

deriving stock instance (ForallDeclX Show x, Show (XId x), Show (XTId x)) => Show (Decl x)

instance (Pretty (XId x), Pretty (XTId x)) => Pretty (Decl x) where
  pPrint (ScDef _ f e) = sep [pPrint f <+> "=", nest 2 $ pPrint e]
  pPrint (ScSig _ f t) = pPrint f <+> "::" <+> pPrint t
  pPrint (DataDef _ d xs cs) =
    sep
      [ "data" <+> pPrint d <+> sep (map pPrint xs) <+> "=",
        nest 2 $ foldl1 (\a b -> sep [a, "|" <+> b]) $ map pprConDef cs
      ]
    where
      pprConDef (con, ts) = pPrint con <+> sep (map (pPrintPrec prettyNormal 12) ts)
  pPrint (Infix _ a o x) = "infix" <> pPrint a <+> pPrint o <+> pPrint x
  pPrint (Foreign _ x t) = "foreign import" <+> pPrint x <+> "::" <+> pPrint t
  pPrint (Import _ name) = "import" <+> pPrint name

------------
-- Module --
------------

data Module x = Module {_moduleName :: ModuleName, _moduleDefinition :: XModule x}

deriving stock instance (ForallDeclX Eq x, Eq (XId x), Eq (XTId x), Eq (XModule x)) => Eq (Module x)

deriving stock instance (ForallDeclX Show x, Show (XId x), Show (XTId x), Show (XModule x)) => Show (Module x)

instance (Pretty (XId x), Pretty (XTId x), Pretty (XModule x)) => Pretty (Module x) where
  pPrint (Module name defs) =
    "module" <+> pPrint name $$ pPrint defs

-- モジュールの循環参照を防ぐため、このモジュールでtype instanceを定義する
type instance XModule (Malgo 'Parse) = [Decl (Malgo 'Parse)]

type instance XModule (Malgo 'Rename) = BindGroup (Malgo 'Rename)

type instance XModule (Malgo 'TypeCheck) = BindGroup (Malgo 'TypeCheck)

type instance XModule (Malgo 'NewTypeCheck) = BindGroup (Malgo 'NewTypeCheck)

type instance XModule (Malgo 'Refine) = BindGroup (Malgo 'Refine)

----------------
-- Bind group --
----------------

data BindGroup x = BindGroup
  { -- | 相互再帰的なグループに分割
    _scDefs :: [[ScDef x]],
    _scSigs :: [ScSig x],
    _dataDefs :: [DataDef x],
    _foreigns :: [Foreign x],
    _imports :: [Import x]
  }

type ScDef x = (XScDef x, XId x, Exp x)

type ScSig x = (XScSig x, XId x, Type x)

type DataDef x = (XDataDef x, XTId x, [XTId x], [(XId x, [Type x])])

type Foreign x = (XForeign x, XId x, Type x)

type Import x = (XImport x, ModuleName)

makeLenses ''BindGroup

deriving stock instance (ForallDeclX Eq x, Eq (XId x), Eq (XTId x)) => Eq (BindGroup x)

deriving stock instance (ForallDeclX Show x, Show (XId x), Show (XTId x)) => Show (BindGroup x)

instance (Pretty (XId x), Pretty (XTId x)) => Pretty (BindGroup x) where
  pPrint BindGroup {_scDefs, _scSigs, _dataDefs, _foreigns} =
    sep $
      punctuate ";" $
        map prettyDataDef _dataDefs
          <> map prettyForeign _foreigns
          <> map prettyScSig _scSigs
          <> concatMap (map prettyScDef) _scDefs
    where
      prettyDataDef (_, d, xs, cs) =
        sep
          [ "data" <+> pPrint d <+> sep (map pPrint xs) <+> "=",
            nest 2 $ foldl1 (\a b -> sep [a, "|" <+> b]) $ map pprConDef cs
          ]
      pprConDef (con, ts) = pPrint con <+> sep (map (pPrintPrec prettyNormal 12) ts)
      prettyForeign (_, x, t) = "foreign import" <+> pPrint x <+> "::" <+> pPrint t
      prettyScSig (_, f, t) = pPrint f <+> "::" <+> pPrint t
      prettyScDef (_, f, e) =
        sep [pPrint f <+> "=", pPrint e]

makeBindGroup :: (Pretty a, Ord (XId x), XId x ~ Id a, Ord (XScDef x)) => [Decl x] -> BindGroup x
makeBindGroup ds =
  BindGroup
    { _scDefs = splitScDef (makeSCC $ mapMaybe scDef ds) (mapMaybe scDef ds),
      _scSigs = mapMaybe scSig ds,
      _dataDefs = mapMaybe dataDef ds,
      _foreigns = mapMaybe foreignDef ds,
      _imports = mapMaybe importDef ds
    }
  where
    scDef (ScDef x f e) = Just (x, f, e)
    scDef _ = Nothing
    scSig (ScSig x f t) = Just (x, f, t)
    scSig _ = Nothing
    dataDef (DataDef x t ps cons) = Just (x, t, ps, cons)
    dataDef _ = Nothing
    foreignDef (Foreign x n t) = Just (x, n, t)
    foreignDef _ = Nothing
    importDef (Import x m) = Just (x, m)
    importDef _ = Nothing
    splitScDef sccs ds = map (mapMaybe (\n -> find (\d -> n == d ^. _2) ds)) sccs

adjacents :: (Ord a, Ord (XId x), XId x ~ Id a1) => (a, XId x, Exp x) -> (XId x, Int, [Int])
adjacents (_, f, e) =
  (f, f ^. idUniq, map (view idUniq) $ toList $ Set.delete f (freevars e))

makeSCC :: (Ord (XId x), XId x ~ Id a1, Ord a) => [(a, XId x, Exp x)] -> [[XId x]]
makeSCC ds = map flattenSCC $ stronglyConnComp adjacents'
  where
    vertices = map (view _2 . adjacents) ds
    adjacents' = map ((\(l, v, vs) -> (l, v, filter (`elem` vertices) vs)) . adjacents) ds
