{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Syntax where

import Data.Int (Int32, Int64)
import qualified Data.Set as Set
import Data.Tuple.Extra (uncurry3)
import Koriel.Pretty
import Language.Griff.Extension
import Language.Griff.Prelude
import Language.Griff.Type (HasType (..))
import qualified Language.Griff.Type as T
import qualified Text.PrettyPrint.HughesPJ as P

-- Unboxed literal

data Unboxed = Int32 Int32 | Int64 Int64 | Float Float | Double Double | Char Char | String String
  deriving stock (Show, Eq, Ord)

instance Pretty Unboxed where
  pPrint (Int32 i) = P.int (fromIntegral i) <> "#"
  pPrint (Int64 i) = P.int (fromIntegral i) <> "L#"
  pPrint (Float f) = P.float f <> "F#"
  pPrint (Double d) = P.double d <> "#"
  pPrint (Char c) = P.quotes (P.char c) <> "#"
  pPrint (String s) = P.doubleQuotes (P.text s) <> "#"

instance HasType Unboxed where
  toType = to $ \case
    Int32 {} -> T.TyPrim T.Int32T
    Int64 {} -> T.TyPrim T.Int64T
    Float {} -> T.TyPrim T.FloatT
    Double {} -> T.TyPrim T.DoubleT
    Char {} -> T.TyPrim T.CharT
    String {} -> T.TyPrim T.StringT
  overType _ = pure

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

deriving stock instance (ForallExpX Eq x, ForallClauseX Eq x, ForallPatX Eq x, ForallStmtX Eq x, Eq (XId x)) => Eq (Exp x)

deriving stock instance (ForallExpX Show x, ForallClauseX Show x, ForallPatX Show x, ForallStmtX Show x, Show (XId x)) => Show (Exp x)

instance (Pretty (XId x)) => Pretty (Exp x) where
  pPrintPrec _ _ (Var _ i) = pPrint i
  pPrintPrec _ _ (Con _ c) = pPrint c
  pPrintPrec _ _ (Unboxed _ u) = pPrint u
  pPrintPrec l d (Apply _ e1 e2) =
    P.maybeParens (d > 10) $ P.sep [pPrintPrec l 10 e1, pPrintPrec l 11 e2]
  pPrintPrec l d (OpApp _ o e1 e2) =
    P.maybeParens (d > 10) $ P.sep [pPrintPrec l 11 e1, pPrintPrec l 10 o <+> pPrintPrec l 11 e2]
  pPrintPrec l _ (Fn _ cs) =
    P.braces $
      P.space
        <> foldl1
          (\a b -> P.sep [a, P.nest (-2) $ "|" <+> b])
          (map (pPrintPrec l 0) cs)
  pPrintPrec _ _ (Tuple _ xs) = P.parens $ P.sep $ P.punctuate "," $ map pPrint xs
  pPrintPrec l _ (Force _ x) = pPrintPrec l 11 x <> "!"

instance (ForallExpX HasType x, ForallClauseX HasType x, ForallPatX HasType x) => HasType (Exp x) where
  toType = to $ \case
    Var x _ -> x ^. toType
    Con x _ -> x ^. toType
    Unboxed x _ -> x ^. toType
    Apply x _ _ -> x ^. toType
    OpApp x _ _ _ -> x ^. toType
    Fn x _ -> x ^. toType
    Tuple x _ -> x ^. toType
    Force x _ -> x ^. toType
  overType f = \case
    Var x v -> Var <$> overType f x <*> pure v
    Con x c -> Con <$> overType f x <*> pure c
    Unboxed x u -> Unboxed <$> overType f x <*> overType f u
    Apply x e1 e2 -> Apply <$> overType f x <*> overType f e1 <*> overType f e2
    OpApp x op e1 e2 -> OpApp <$> overType f x <*> pure op <*> overType f e1 <*> overType f e2
    Fn x cs -> Fn <$> overType f x <*> traverse (overType f) cs
    Tuple x es -> Tuple <$> overType f x <*> traverse (overType f) es
    Force x e -> Force <$> overType f x <*> overType f e

freevars :: Ord (XId x) => Exp x -> Set (XId x)
freevars (Var _ v) = Set.singleton v
freevars (Con _ _) = Set.empty
freevars (Unboxed _ _) = Set.empty
freevars (Apply _ e1 e2) = freevars e1 <> freevars e2
freevars (OpApp _ op e1 e2) = Set.insert op $ freevars e1 <> freevars e2
freevars (Fn _ cs) = mconcat $ map freevarsClause cs
freevars (Tuple _ es) = mconcat $ map freevars es
freevars (Force _ e) = freevars e

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

instance (ForallExpX HasType x, ForallClauseX HasType x, ForallPatX HasType x) => HasType (Stmt x) where
  toType = to $ \case
    Let _ _ e -> view toType e
    NoBind _ e -> view toType e
  overType f = \case
    Let x v e -> Let x v <$> overType f e
    NoBind x e -> NoBind x <$> overType f e

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
  pPrintPrec l _ (Clause _ ps e) = sep [P.sep (map (pPrintPrec l 11) ps) <+> "->", sep (punctuate ";" $ map pPrint e)]

instance (ForallExpX HasType x, ForallClauseX HasType x, ForallPatX HasType x) => HasType (Clause x) where
  toType = to $ \(Clause x _ _) -> view toType x
  overType f (Clause x ps e) =
    Clause <$> overType f x <*> traverse (overType f) ps <*> traverse (overType f) e

freevarsClause :: Ord (XId x) => Clause x -> Set (XId x)
freevarsClause (Clause _ pats es) = foldMap freevarsStmt es Set.\\ mconcat (map bindVars pats)

-------------
-- Pattern --
-------------

data Pat x
  = VarP (XVarP x) (XId x)
  | ConP (XConP x) (XId x) [Pat x]
  | TupleP (XTupleP x) [Pat x]
  | UnboxedP (XUnboxedP x) Unboxed

deriving stock instance (ForallPatX Eq x, Eq (XId x)) => Eq (Pat x)

deriving stock instance (ForallPatX Show x, Show (XId x)) => Show (Pat x)

deriving stock instance (ForallPatX Ord x, Ord (XId x)) => Ord (Pat x)

instance (Pretty (XId x)) => Pretty (Pat x) where
  pPrintPrec _ _ (VarP _ i) = pPrint i
  pPrintPrec _ _ (ConP _ i []) = pPrint i
  pPrintPrec l d (ConP _ i ps) =
    P.maybeParens (d > 10) $ pPrint i <+> P.sep (map (pPrintPrec l 11) ps)
  pPrintPrec _ _ (TupleP _ ps) =
    parens $ sep $ punctuate "," $ map pPrint ps
  pPrintPrec _ _ (UnboxedP _ u) = pPrint u

instance (ForallPatX HasType x) => HasType (Pat x) where
  toType = to $ \case
    VarP x _ -> view toType x
    ConP x _ _ -> view toType x
    TupleP x _ -> view toType x
    UnboxedP x _ -> view toType x
  overType f = \case
    VarP x v -> VarP <$> overType f x <*> pure v
    ConP x c ps -> ConP <$> overType f x <*> pure c <*> traverse (overType f) ps
    TupleP x ps -> TupleP <$> overType f x <*> traverse (overType f) ps
    UnboxedP x u -> UnboxedP <$> overType f x <*> overType f u

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

_UnboxedP :: Prism' (Pat x) (XUnboxedP x, Unboxed)
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
    P.maybeParens (d > 11) $ pPrint t <+> P.sep (map (pPrintPrec l 12) ts)
  pPrintPrec _ _ (TyVar _ i) = pPrint i
  pPrintPrec _ _ (TyCon _ i) = pPrint i
  pPrintPrec l d (TyArr _ t1 t2) =
    P.maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TyTuple _ ts) = P.parens $ P.sep $ P.punctuate "," $ map pPrint ts
  pPrintPrec _ _ (TyLazy _ t) = P.braces $ pPrint t

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
  = ScDef (XScDef x) (XId x) [XId x] (Exp x)
  | ScSig (XScSig x) (XId x) (Type x)
  | DataDef (XDataDef x) (XTId x) [XTId x] [(XId x, [Type x])]
  | Infix (XInfix x) Assoc Int (XId x)
  | Foreign (XForeign x) (XId x) (Type x)
  | Import (XImport x) ModuleName

deriving stock instance (ForallDeclX Eq x, Eq (XId x), Eq (XTId x)) => Eq (Decl x)

deriving stock instance (ForallDeclX Show x, Show (XId x), Show (XTId x)) => Show (Decl x)

instance (Pretty (XId x), Pretty (XTId x)) => Pretty (Decl x) where
  pPrint (ScDef _ f xs e) = P.sep [pPrint f <+> P.sep (map pPrint xs) <+> "=", P.nest 2 $ pPrint e]
  pPrint (ScSig _ f t) = pPrint f <+> "::" <+> pPrint t
  pPrint (DataDef _ d xs cs) =
    P.sep
      [ "data" <+> pPrint d <+> P.sep (map pPrint xs) <+> "=",
        P.nest 2 $ foldl1 (\a b -> P.sep [a, "|" <+> b]) $ map pprConDef cs
      ]
    where
      pprConDef (con, ts) = pPrint con <+> P.sep (map (pPrintPrec prettyNormal 12) ts)
  pPrint (Infix _ a o x) = "infix" <> pPrint a <+> pPrint o <+> pPrint x
  pPrint (Foreign _ x t) = "foreign import" <+> pPrint x <+> "::" <+> pPrint t
  pPrint (Import _ name) = "import" <+> pPrint name

------------
-- Module --
------------

data Module x = Module {_moduleName :: ModuleName, _moduleDecls :: [Decl x]}

deriving stock instance (ForallDeclX Eq x, Eq (XId x), Eq (XTId x)) => Eq (Module x)

deriving stock instance (ForallDeclX Show x, Show (XId x), Show (XTId x)) => Show (Module x)

instance (Pretty (XId x), Pretty (XTId x)) => Pretty (Module x) where
  pPrint (Module name ds) =
    "module" <+> pPrint name <> ";"
      $$ sep (map (\d -> pPrint d <> ";") ds)