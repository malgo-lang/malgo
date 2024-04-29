{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Syntax
  ( Literal (..),
    Type (..),
    Expr (..),
    Stmt (..),
    Clause (..),
    Pat (..),
    _VarP,
    _ConP,
    _TupleP,
    _RecordP,
    _ListP,
    _UnboxedP,
    _BoxedP,
    Decl (..),
    ParsedDefinitions (..),
    BindGroup (..),
    ScDef,
    ScSig,
    DataDef,
    TypeSynonym,
    Foreign,
    Import,
    scDefs,
    scSigs,
    dataDefs,
    typeSynonyms,
    foreigns,
    imports,
    Module (..),
    toUnboxed,
    getTyVars,
    makeBindGroup,
  )
where

import Control.Lens (makeLenses, makePrisms, view, (^.), _2)
import Data.Graph (flattenSCC, stronglyConnComp)
import Data.HashSet qualified as HashSet
import Koriel.Id
import Malgo.Infer.TypeRep hiding (TyApp, TyArr, TyCon, TyRecord, TyTuple, TyVar, Type, freevars)
import Malgo.Prelude hiding (All)
import Malgo.Syntax.Extension

sexpr :: [Doc ann] -> Doc ann
sexpr = parens . sep

-- | Unboxed and boxed literal
data Literal x = Int32 Int32 | Int64 Int64 | Float Float | Double Double | Char Char | String Text
  deriving stock (Show, Eq, Ord)

instance Pretty (Literal x) where
  pretty (Int32 i) = sexpr ["int32", pretty (toInteger i)]
  pretty (Int64 i) = sexpr ["int64", pretty (toInteger i)]
  pretty (Float f) = sexpr ["float", pretty f]
  pretty (Double d) = sexpr ["double", pretty d]
  pretty (Char c) = sexpr ["char", squotes (pretty c)]
  pretty (String s) = sexpr ["string", dquotes (pretty s)]

instance HasType (Literal x) where
  typeOf Int32 {} = TyPrim Int32T
  typeOf Int64 {} = TyPrim Int64T
  typeOf Float {} = TyPrim FloatT
  typeOf Double {} = TyPrim DoubleT
  typeOf Char {} = TyPrim CharT
  typeOf String {} = TyPrim StringT
  types f v = f (typeOf v) $> v

toUnboxed :: Literal Boxed -> Literal Unboxed
toUnboxed = coerce

-- * Type

data Type x
  = TyApp (XTyApp x) (Type x) [Type x]
  | TyVar (XTyVar x) (XId x)
  | TyCon (XTyCon x) (XId x)
  | TyArr (XTyArr x) (Type x) (Type x)
  | TyTuple (XTyTuple x) [Type x]
  | TyRecord (XTyRecord x) [(Text, Type x)]
  | TyBlock (XTyBlock x) (Type x)

deriving stock instance (ForallTypeX Eq x, Eq (XId x)) => Eq (Type x)

deriving stock instance (ForallTypeX Show x, Show (XId x)) => Show (Type x)

instance (Pretty (XId x)) => Pretty (Type x) where
  pretty (TyApp _ t ts) = sexpr $ ["app", pretty t] <> map pretty ts
  pretty (TyVar _ v) = pretty v
  pretty (TyCon _ c) = pretty c
  pretty (TyArr _ t1 t2) = sexpr ["->", pretty t1, pretty t2]
  pretty (TyTuple _ ts) = sexpr $ "tuple" : map pretty ts
  pretty (TyRecord _ kvs) = sexpr $ "record" : map (\(k, v) -> sexpr [pretty k, pretty v]) kvs
  pretty (TyBlock _ t) = sexpr ["block", pretty t]

getTyVars :: (Hashable (XId x)) => Type x -> HashSet (XId x)
getTyVars (TyApp _ t ts) = getTyVars t <> mconcat (map getTyVars ts)
getTyVars (TyVar _ v) = HashSet.singleton v
getTyVars TyCon {} = mempty
getTyVars (TyArr _ t1 t2) = getTyVars t1 <> getTyVars t2
getTyVars (TyTuple _ ts) = mconcat $ map getTyVars ts
getTyVars (TyRecord _ kvs) = mconcat $ map (getTyVars . snd) kvs
getTyVars (TyBlock _ t) = getTyVars t

-- * Expression

data Expr x
  = Var (XVar x) (XId x)
  | Unboxed (XUnboxed x) (Literal Unboxed)
  | Boxed (XBoxed x) (Literal Boxed)
  | Apply (XApply x) (Expr x) (Expr x)
  | OpApp (XOpApp x) (XId x) (Expr x) (Expr x)
  | Fn (XFn x) (NonEmpty (Clause x))
  | Tuple (XTuple x) [Expr x]
  | Record (XRecord x) [(Text, Expr x)]
  | List (XList x) [Expr x]
  | Ann (XAnn x) (Expr x) (Type x)
  | Seq (XSeq x) (NonEmpty (Stmt x))
  | Parens (XParens x) (Expr x)

deriving stock instance (ForallExpX Eq x, ForallClauseX Eq x, ForallPatX Eq x, ForallStmtX Eq x, ForallTypeX Eq x, Eq (XId x)) => Eq (Expr x)

deriving stock instance (ForallExpX Show x, ForallClauseX Show x, ForallPatX Show x, ForallStmtX Show x, ForallTypeX Show x, Show (XId x)) => Show (Expr x)

instance (Pretty (XId x)) => Pretty (Expr x) where
  pretty (Var _ id) = pretty id
  pretty (Unboxed _ l) = sexpr ["unboxed", pretty l]
  pretty (Boxed _ l) = sexpr ["boxed", pretty l]
  pretty (Apply _ e1 e2) = sexpr ["apply", pretty e1, pretty e2]
  pretty (OpApp _ op e1 e2) = sexpr ["opapp", pretty op, pretty e1, pretty e2]
  pretty (Fn _ cs) = sexpr ["fn", sexpr $ map pretty (toList cs)]
  pretty (Tuple _ es) = sexpr $ "tuple" : map pretty es
  pretty (Record _ kvs) = sexpr $ "record" : map (\(k, v) -> sexpr [pretty k, pretty v]) kvs
  pretty (List _ es) = sexpr $ "list" : map pretty es
  pretty (Ann _ e t) = sexpr ["ann", pretty e, pretty t]
  pretty (Seq _ ss) = sexpr $ "seq" : map pretty (toList ss)
  pretty (Parens _ e) = sexpr ["parens", pretty e]

instance
  (ForallExpX HasType x, ForallClauseX HasType x, ForallPatX HasType x) =>
  HasType (Expr x)
  where
  typeOf (Var x _) = typeOf x
  typeOf (Unboxed x _) = typeOf x
  typeOf (Boxed x _) = typeOf x
  typeOf (Apply x _ _) = typeOf x
  typeOf (OpApp x _ _ _) = typeOf x
  typeOf (Fn x _) = typeOf x
  typeOf (Tuple x _) = typeOf x
  typeOf (Record x _) = typeOf x
  typeOf (List x _) = typeOf x
  typeOf (Ann x _ _) = typeOf x
  typeOf (Seq x _) = typeOf x
  typeOf (Parens x _) = typeOf x

  types f = \case
    Var x v -> Var <$> types f x <*> pure v
    Unboxed x u -> Unboxed <$> types f x <*> types f u
    Boxed x b -> Boxed <$> types f x <*> types f b
    Apply x e1 e2 -> Apply <$> types f x <*> types f e1 <*> types f e2
    OpApp x op e1 e2 -> OpApp <$> types f x <*> pure op <*> types f e1 <*> types f e2
    Fn x cs -> Fn <$> types f x <*> traverse (types f) cs
    Tuple x es -> Tuple <$> types f x <*> traverse (types f) es
    Record x kvs -> Record <$> types f x <*> traverse (\(k, v) -> (k,) <$> types f v) kvs
    List x es -> List <$> types f x <*> traverse (types f) es
    Ann x e t -> Ann <$> types f x <*> types f e <*> pure t
    Seq x ss -> Seq <$> types f x <*> traverse (types f) ss
    Parens x e -> Parens <$> types f x <*> types f e

freevars :: (Hashable (XId x)) => Expr x -> HashSet (XId x)
freevars (Var _ v) = HashSet.singleton v
freevars (Unboxed _ _) = mempty
freevars (Boxed _ _) = mempty
freevars (Apply _ e1 e2) = freevars e1 <> freevars e2
freevars (OpApp _ op e1 e2) = HashSet.insert op $ freevars e1 <> freevars e2
freevars (Fn _ cs) = foldMap freevarsClause cs
  where
    freevarsClause :: (Hashable (XId x)) => Clause x -> HashSet (XId x)
    freevarsClause (Clause _ pats e) = HashSet.difference (freevars e) (mconcat (map bindVars pats))
    bindVars (VarP _ x) = HashSet.singleton x
    bindVars (ConP _ _ ps) = mconcat $ map bindVars ps
    bindVars (TupleP _ ps) = mconcat $ map bindVars ps
    bindVars (RecordP _ kps) = mconcat $ map (bindVars . snd) kps
    bindVars (ListP _ ps) = mconcat $ map bindVars ps
    bindVars UnboxedP {} = mempty
    bindVars BoxedP {} = mempty
freevars (Tuple _ es) = mconcat $ map freevars es
freevars (Record _ kvs) = mconcat $ map (freevars . snd) kvs
freevars (List _ es) = mconcat $ map freevars es
freevars (Ann _ e _) = freevars e
freevars (Seq _ ss) = freevarsStmts ss
  where
    freevarsStmts (Let _ x e :| ss) = freevars e <> HashSet.delete x (freevarsStmts' ss)
    freevarsStmts (With _ Nothing e :| ss) = freevars e <> freevarsStmts' ss
    freevarsStmts (With _ (Just x) e :| ss) = freevars e <> HashSet.delete x (freevarsStmts' ss)
    freevarsStmts (NoBind _ e :| ss) = freevars e <> freevarsStmts' ss
    freevarsStmts' [] = mempty
    freevarsStmts' (s : ss) = freevarsStmts (s :| ss)
freevars (Parens _ e) = freevars e

-- * Stmt

data Stmt x
  = Let (XLet x) (XId x) (Expr x)
  | With (XWith x) (Maybe (XId x)) (Expr x)
  | NoBind (XNoBind x) (Expr x)

deriving stock instance (ForallClauseX Eq x, ForallPatX Eq x, ForallExpX Eq x, ForallStmtX Eq x, ForallTypeX Eq x, Eq (XId x)) => Eq (Stmt x)

deriving stock instance (ForallClauseX Show x, ForallPatX Show x, ForallExpX Show x, ForallStmtX Show x, ForallTypeX Show x, Show (XId x)) => Show (Stmt x)

instance (Pretty (XId x)) => Pretty (Stmt x) where
  pretty (Let _ var body) = sexpr ["let", pretty var, pretty body]
  pretty (With _ Nothing body) = sexpr ["with", pretty body]
  pretty (With _ (Just var) body) = sexpr ["with", pretty var, pretty body]
  pretty (NoBind _ body) = sexpr ["do", pretty body]

instance
  (ForallExpX HasType x, ForallClauseX HasType x, ForallPatX HasType x) =>
  HasType (Stmt x)
  where
  typeOf (Let _ _ e) = typeOf e
  typeOf (With _ _ e) = typeOf e
  typeOf (NoBind _ e) = typeOf e

  types f = \case
    Let x v e -> Let x v <$> types f e
    With x v e -> With x v <$> types f e
    NoBind x e -> NoBind x <$> types f e

-- * Clause

data Clause x = Clause (XClause x) [Pat x] (Expr x)

deriving stock instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x, ForallStmtX Eq x, ForallTypeX Eq x, Eq (XId x)) => Eq (Clause x)

deriving stock instance (ForallClauseX Show x, ForallExpX Show x, ForallPatX Show x, ForallStmtX Show x, ForallTypeX Show x, Show (XId x)) => Show (Clause x)

instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x, Ord (XId x), ForallPatX Ord x, ForallStmtX Ord x, ForallTypeX Ord x) => Ord (Clause x) where
  (Clause _ ps1 _) `compare` (Clause _ ps2 _) = ps1 `compare` ps2

instance (Pretty (XId x)) => Pretty (Clause x) where
  pretty (Clause _ pats body) = sexpr ["clause", sexpr $ map pretty pats, pretty body]

instance
  (ForallClauseX HasType x, ForallPatX HasType x, ForallExpX HasType x) =>
  HasType (Clause x)
  where
  typeOf (Clause x _ _) = typeOf x

  types f (Clause x ps e) = Clause <$> types f x <*> traverse (types f) ps <*> types f e

-- * Pattern

data Pat x
  = VarP (XVarP x) (XId x)
  | ConP (XConP x) (XId x) [Pat x]
  | TupleP (XTupleP x) [Pat x]
  | RecordP (XRecordP x) [(Text, Pat x)]
  | ListP (XListP x) [Pat x]
  | UnboxedP (XUnboxedP x) (Literal Unboxed)
  | BoxedP (XBoxedP x) (Literal Boxed)

deriving stock instance (ForallPatX Eq x, Eq (XId x)) => Eq (Pat x)

deriving stock instance (ForallPatX Show x, Show (XId x)) => Show (Pat x)

deriving stock instance (ForallPatX Ord x, Ord (XId x)) => Ord (Pat x)

instance (Pretty (XId x)) => Pretty (Pat x) where
  pretty (VarP _ id) = pretty id
  pretty (ConP _ id ps) = sexpr $ ["con", pretty id] <> map pretty ps
  pretty (TupleP _ ps) = sexpr $ "tuple" : map pretty ps
  pretty (RecordP _ kps) = sexpr $ "record" : map (\(k, p) -> sexpr [pretty k, pretty p]) kps
  pretty (ListP _ ps) = sexpr $ "list" : map pretty ps
  pretty (UnboxedP _ l) = sexpr ["unboxed", pretty l]
  pretty (BoxedP _ l) = sexpr ["boxed", pretty l]

instance
  (ForallPatX HasType x) =>
  HasType (Pat x)
  where
  typeOf (VarP x _) = typeOf x
  typeOf (ConP x _ _) = typeOf x
  typeOf (TupleP x _) = typeOf x
  typeOf (RecordP x _) = typeOf x
  typeOf (ListP x _) = typeOf x
  typeOf (UnboxedP x _) = typeOf x
  typeOf (BoxedP x _) = typeOf x

  types f = \case
    VarP x v -> VarP <$> types f x <*> pure v
    ConP x c ps -> ConP <$> types f x <*> pure c <*> traverse (types f) ps
    TupleP x ps -> TupleP <$> types f x <*> traverse (types f) ps
    RecordP x kps -> RecordP <$> types f x <*> traverse (bitraverse pure (types f)) kps
    ListP x ps -> ListP <$> types f x <*> traverse (types f) ps
    UnboxedP x u -> UnboxedP <$> types f x <*> types f u
    BoxedP x b -> BoxedP <$> types f x <*> types f b

makePrisms ''Pat

-- * Declaration

data Decl x
  = ScDef (XScDef x) (XId x) (Expr x)
  | ScSig (XScSig x) (XId x) (Type x)
  | DataDef (XDataDef x) (XId x) [(Range, XId x)] [(Range, XId x, [Type x])]
  | TypeSynonym (XTypeSynonym x) (XId x) [XId x] (Type x)
  | Infix (XInfix x) Assoc Int (XId x)
  | Foreign (XForeign x) (XId x) (Type x)
  | Import (XImport x) ModuleName ImportList

deriving stock instance (ForallDeclX Eq x, Eq (XId x)) => Eq (Decl x)

deriving stock instance (ForallDeclX Show x, Show (XId x)) => Show (Decl x)

instance (Pretty (XId x)) => Pretty (Decl x) where
  pretty (ScDef x name expr) = prettyScDef (x, name, expr)
  pretty (ScSig x name ty) = prettyScSig (x, name, ty)
  pretty (DataDef x name params cons) = prettyDataDef (x, name, params, cons)
  pretty (TypeSynonym x name params ty) = prettyTypeSynonym (x, name, params, ty)
  pretty (Infix _ assoc prec op) = sexpr ["infix", pretty assoc, pretty prec, pretty op]
  pretty (Foreign x name ty) = prettyForeign (x, name, ty)
  pretty (Import x name list) = prettyImport (x, name, list)

prettyScDef :: (Pretty (XId x)) => ScDef x -> Doc ann
prettyScDef (_, f, e) = sexpr ["def", pretty f, pretty e]

prettyScSig :: (Pretty (XId x)) => ScSig x -> Doc ann
prettyScSig (_, f, t) = sexpr ["sig", pretty f, pretty t]

prettyDataDef :: (Pretty (XId x)) => DataDef x -> Doc ann
prettyDataDef (_, t, ps, cons) =
  sexpr
    [ "data",
      pretty t,
      sexpr $ map (pretty . view _2) ps,
      sexpr $ map prettyConDef cons
    ]
  where
    prettyConDef (_, con, ts) = sexpr [pretty con, sexpr $ map pretty ts]

prettyTypeSynonym :: (Pretty (XId x)) => TypeSynonym x -> Doc ann
prettyTypeSynonym (_, t, ps, ty) = sexpr ["type", pretty t, sexpr $ map pretty ps, pretty ty]

prettyForeign :: (Pretty (XId x)) => Foreign x -> Doc ann
prettyForeign (_, n, t) = sexpr ["foreign", pretty n, pretty t]

-- prettyImport :: Import x -> Doc ann
prettyImport :: (x, ModuleName, ImportList) -> Doc ann
prettyImport (_, m, list) =
  sexpr ["import", pretty m, prettyImportList list]
  where
    prettyImportList All = "all"
    prettyImportList (Selected xs) = sexpr $ "selected" : map pretty xs
    prettyImportList (As m) = sexpr ["as", pretty m]

-- * Module

data Module x = Module {moduleName :: ModuleName, moduleDefinition :: XModule x}

deriving stock instance (ForallDeclX Eq x, Eq (XId x), Eq (XModule x)) => Eq (Module x)

deriving stock instance (ForallDeclX Show x, Show (XId x), Show (XModule x)) => Show (Module x)

instance (Pretty (XId x), Pretty (XModule x)) => Pretty (Module x) where
  pretty (Module name defs) =
    sexpr ["module", pretty name, pretty defs]

newtype ParsedDefinitions = ParsedDefinitions [Decl (Malgo 'Parse)]
  deriving stock (Eq, Show)

instance Pretty ParsedDefinitions where
  pretty (ParsedDefinitions ds) = sep $ map pretty ds

-- モジュールの循環参照を防ぐため、このモジュールでtype instanceを定義する
type instance XModule (Malgo 'Parse) = ParsedDefinitions

type instance XModule (Malgo 'Rename) = BindGroup (Malgo 'Rename)

type instance XModule (Malgo 'Infer) = BindGroup (Malgo 'Infer)

type instance XModule (Malgo 'Refine) = BindGroup (Malgo 'Refine)

-- * Bind group

data BindGroup x = BindGroup
  { -- | 相互再帰的なグループに分割
    _scDefs :: [[ScDef x]],
    _scSigs :: [ScSig x],
    _dataDefs :: [DataDef x],
    _typeSynonyms :: [TypeSynonym x],
    _foreigns :: [Foreign x],
    _imports :: [Import x]
  }

type ScDef x = (XScDef x, XId x, Expr x)

type ScSig x = (XScSig x, XId x, Type x)

type DataDef x = (XDataDef x, XId x, [(Range, XId x)], [(Range, XId x, [Type x])])

type TypeSynonym x = (XTypeSynonym x, XId x, [XId x], Type x)

type Foreign x = (XForeign x, XId x, Type x)

type Import x = (XImport x, ModuleName, ImportList)

makeLenses ''BindGroup

deriving stock instance (ForallDeclX Eq x, Eq (XId x)) => Eq (BindGroup x)

deriving stock instance (ForallDeclX Show x, Show (XId x)) => Show (BindGroup x)

instance (Pretty (XId x)) => Pretty (BindGroup x) where
  pretty BindGroup {..} =
    sep
      [ sexpr $ map prettyScDefs _scDefs,
        sexpr $ map prettyScSig _scSigs,
        sexpr $ map prettyDataDef _dataDefs,
        sexpr $ map prettyTypeSynonym _typeSynonyms,
        sexpr $ map prettyForeign _foreigns,
        sexpr $ map prettyImport _imports
      ]
    where
      prettyScDefs = sexpr . map prettyScDef

makeBindGroup :: (Hashable (XId x), Ord (XId x)) => [Decl x] -> BindGroup x
makeBindGroup ds =
  BindGroup
    { _scDefs = splitScDef (makeSCC $ mapMaybe scDef ds) (mapMaybe scDef ds),
      _scSigs = mapMaybe scSig ds,
      _dataDefs = mapMaybe dataDef ds,
      _typeSynonyms = mapMaybe typeSynonym ds,
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
    typeSynonym (TypeSynonym x t ps t') = Just (x, t, ps, t')
    typeSynonym _ = Nothing
    foreignDef (Foreign x n t) = Just (x, n, t)
    foreignDef _ = Nothing
    importDef (Import x m ns) = Just (x, m, ns)
    importDef _ = Nothing
    splitScDef sccs ds = map (mapMaybe (\n -> find (\d -> n == d ^. _2) ds)) sccs

adjacents :: (Hashable (XId x)) => (a, XId x, Expr x) -> (XId x, XId x, [XId x])
adjacents (_, f, e) =
  (f, f, toList $ HashSet.delete f (freevars e))

makeSCC :: (Hashable (XId x), Ord (XId x)) => [(a, XId x, Expr x)] -> [[XId x]]
makeSCC ds = map flattenSCC $ stronglyConnComp adjacents'
  where
    vertices = map (view _2 . adjacents) ds
    adjacents' = map ((\(l, v, vs) -> (l, v, filter (`elem` vertices) vs)) . adjacents) ds
