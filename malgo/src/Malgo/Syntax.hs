{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Syntax where

import Data.Coerce (coerce)
import Data.Foldable (find, foldl1)
import Data.Graph (flattenSCC, stronglyConnComp)
import qualified Data.HashSet as HashSet
import Koriel.Id
import Koriel.Pretty
import Malgo.Prelude
import Malgo.Syntax.Extension
import qualified Malgo.TypeRep.Static as S
import qualified Malgo.TypeRep.UTerm as U
import qualified Malgo.UTerm as U
import qualified RIO.NonEmpty as NonEmpty

-- | Unboxed and literal
data Literal x = Int32 Int32 | Int64 Int64 | Float Float | Double Double | Char Char | String String
  deriving stock (Show, Eq, Ord)

instance Pretty (Literal x) where
  pretty (Int32 i) = pretty (toInteger i)
  pretty (Int64 i) = pretty (toInteger i) <> "L"
  pretty (Float f) = pretty f <> "F"
  pretty (Double d) = pretty d
  pretty (Char c) = squotes (pretty c)
  pretty (String s) = dquotes (pretty s)

instance U.HasType (Literal x) where
  typeOf Int32 {} = U.TyPrim S.Int32T
  typeOf Int64 {} = U.TyPrim S.Int64T
  typeOf Float {} = U.TyPrim S.FloatT
  typeOf Double {} = U.TyPrim S.DoubleT
  typeOf Char {} = U.TyPrim S.CharT
  typeOf String {} = U.TyPrim S.StringT

instance S.HasType (Literal x) where
  typeOf Int32 {} = S.TyPrim S.Int32T
  typeOf Int64 {} = S.TyPrim S.Int64T
  typeOf Float {} = S.TyPrim S.FloatT
  typeOf Double {} = S.TyPrim S.DoubleT
  typeOf Char {} = S.TyPrim S.CharT
  typeOf String {} = S.TyPrim S.StringT

instance U.HasUTerm S.TypeF U.TypeVar (Literal x) where
  walkOn f v = f (U.typeOf v) >> pure v

toUnboxed :: Literal Boxed -> Literal Unboxed
toUnboxed = coerce

-- Expression

data Exp x
  = Var (XVar x) (WithPrefix (XId x))
  | Unboxed (XUnboxed x) (Literal Unboxed)
  | Boxed (XBoxed x) (Literal Boxed)
  | Apply (XApply x) (Exp x) (Exp x)
  | OpApp (XOpApp x) (XId x) (Exp x) (Exp x)
  | Fn (XFn x) (NonEmpty (Clause x))
  | Tuple (XTuple x) [Exp x]
  | Record (XRecord x) [(WithPrefix (XId x), Exp x)]
  | List (XList x) [Exp x]
  | Force (XForce x) (Exp x)
  | RecordAccess (XRecordAccess x) (WithPrefix (XId x))
  | Parens (XParens x) (Exp x)

deriving stock instance (ForallExpX Eq x, ForallClauseX Eq x, ForallPatX Eq x, ForallStmtX Eq x, Eq (XId x)) => Eq (Exp x)

deriving stock instance (ForallExpX Show x, ForallClauseX Show x, ForallPatX Show x, ForallStmtX Show x, Show (XId x)) => Show (Exp x)

instance (Pretty (XId x)) => Pretty (Exp x) where
  pretty x = pprExpPrec 0 x

pprExpPrec :: (Pretty (XId x)) => Int -> Exp x -> Doc ann
pprExpPrec _ (Var _ i) = pretty i
pprExpPrec _ (Unboxed _ lit) = pretty lit <> "#"
pprExpPrec _ (Boxed _ lit) = pretty lit
pprExpPrec d (Apply _ e1 e2) =
  maybeParens (d > 10) $ sep [pprExpPrec 10 e1, pprExpPrec 11 e2]
pprExpPrec d (OpApp _ o e1 e2) =
  maybeParens (d > 10) $ sep [pprExpPrec 11 e1, pretty o <+> pprExpPrec 11 e2]
pprExpPrec _ (Fn _ cs) =
  braces $
    space
      <> foldl1
        (\a b -> sep [a, nest (-2) $ "|" <+> b])
        (fmap pretty cs)
pprExpPrec _ (Tuple _ xs) = parens $ sep $ punctuate "," $ map pretty xs
pprExpPrec _ (Record _ kvs) = braces $ sep $ punctuate "," $ map (\(k, v) -> pretty k <> ":" <+> pretty v) kvs
pprExpPrec _ (List _ xs) = brackets $ sep $ punctuate "," $ map pretty xs
pprExpPrec _ (Force _ x) = "!" <> pprExpPrec 11 x
pprExpPrec _ (RecordAccess _ x) = "#" <> pretty x
pprExpPrec _ (Parens _ x) = parens $ pretty x

instance
  ForallExpX U.WithUType x =>
  U.HasType (Exp x)
  where
  typeOf (Var x _) = x ^. U.withUType
  typeOf (Unboxed x _) = x ^. U.withUType
  typeOf (Boxed x _) = x ^. U.withUType
  typeOf (Apply x _ _) = x ^. U.withUType
  typeOf (OpApp x _ _ _) = x ^. U.withUType
  typeOf (Fn x _) = x ^. U.withUType
  typeOf (Tuple x _) = x ^. U.withUType
  typeOf (Record x _) = x ^. U.withUType
  typeOf (List x _) = x ^. U.withUType
  typeOf (Force x _) = x ^. U.withUType
  typeOf (RecordAccess x _) = x ^. U.withUType
  typeOf (Parens x _) = x ^. U.withUType

instance
  ForallExpX S.WithType x =>
  S.HasType (Exp x)
  where
  typeOf (Var x _) = x ^. S.withType
  typeOf (Unboxed x _) = x ^. S.withType
  typeOf (Boxed x _) = x ^. S.withType
  typeOf (Apply x _ _) = x ^. S.withType
  typeOf (OpApp x _ _ _) = x ^. S.withType
  typeOf (Fn x _) = x ^. S.withType
  typeOf (Tuple x _) = x ^. S.withType
  typeOf (Record x _) = x ^. S.withType
  typeOf (List x _) = x ^. S.withType
  typeOf (Force x _) = x ^. S.withType
  typeOf (RecordAccess x _) = x ^. S.withType
  typeOf (Parens x _) = x ^. S.withType

instance
  ( ForallExpX (U.HasUTerm S.TypeF U.TypeVar) x,
    ForallClauseX (U.HasUTerm S.TypeF U.TypeVar) x,
    ForallPatX (U.HasUTerm S.TypeF U.TypeVar) x
  ) =>
  U.HasUTerm S.TypeF U.TypeVar (Exp x)
  where
  walkOn f = \case
    Var x v -> Var <$> U.walkOn f x <*> pure v
    Unboxed x u -> Unboxed <$> U.walkOn f x <*> U.walkOn f u
    Boxed x b -> Boxed <$> U.walkOn f x <*> U.walkOn f b
    Apply x e1 e2 -> Apply <$> U.walkOn f x <*> U.walkOn f e1 <*> U.walkOn f e2
    OpApp x op e1 e2 -> OpApp <$> U.walkOn f x <*> pure op <*> U.walkOn f e1 <*> U.walkOn f e2
    Fn x cs -> Fn <$> U.walkOn f x <*> traverse (U.walkOn f) cs
    Tuple x es -> Tuple <$> U.walkOn f x <*> traverse (U.walkOn f) es
    Record x kvs -> Record <$> U.walkOn f x <*> traverse (\(k, v) -> (k,) <$> U.walkOn f v) kvs
    List x es -> List <$> U.walkOn f x <*> traverse (U.walkOn f) es
    Force x e -> Force <$> U.walkOn f x <*> U.walkOn f e
    RecordAccess x l -> RecordAccess <$> U.walkOn f x <*> pure l
    Parens x e -> Parens <$> U.walkOn f x <*> U.walkOn f e

freevars :: (Eq (XId x), Hashable (XId x)) => Exp x -> HashSet (XId x)
freevars (Var _ (WithPrefix v)) = HashSet.singleton (v ^. value)
freevars (Unboxed _ _) = mempty
freevars (Boxed _ _) = mempty
freevars (Apply _ e1 e2) = freevars e1 <> freevars e2
freevars (OpApp _ op e1 e2) = HashSet.insert op $ freevars e1 <> freevars e2
freevars (Fn _ cs) = foldMap freevarsClause cs
freevars (Tuple _ es) = mconcat $ map freevars es
freevars (Record _ kvs) = mconcat $ map (freevars . snd) kvs
freevars (List _ es) = mconcat $ map freevars es
freevars (Force _ e) = freevars e
freevars (RecordAccess _ _) = mempty
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
  pretty (Let _ v e) = "let" <+> pretty v <+> "=" <+> pretty e
  pretty (NoBind _ e) = pretty e

instance
  ForallExpX U.WithUType x =>
  U.HasType (Stmt x)
  where
  typeOf (Let _ _ e) = U.typeOf e
  typeOf (NoBind _ e) = U.typeOf e

instance
  ForallExpX S.WithType x =>
  S.HasType (Stmt x)
  where
  typeOf (Let _ _ e) = S.typeOf e
  typeOf (NoBind _ e) = S.typeOf e

instance
  ( ForallExpX (U.HasUTerm S.TypeF U.TypeVar) x,
    ForallClauseX (U.HasUTerm S.TypeF U.TypeVar) x,
    ForallPatX (U.HasUTerm S.TypeF U.TypeVar) x
  ) =>
  U.HasUTerm S.TypeF U.TypeVar (Stmt x)
  where
  walkOn f = \case
    Let x v e -> Let x v <$> U.walkOn f e
    NoBind x e -> NoBind x <$> U.walkOn f e

freevarsStmt :: (Eq (XId x), Hashable (XId x)) => Stmt x -> HashSet (XId x)
freevarsStmt (Let _ x e) = HashSet.delete x $ freevars e
freevarsStmt (NoBind _ e) = freevars e

-- [Exp x]は、末尾へのアクセスが速いものに変えたほうが良いかも
data Clause x = Clause (XClause x) [Pat x] (NonEmpty (Stmt x))

deriving stock instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x, ForallStmtX Eq x, Eq (XId x)) => Eq (Clause x)

deriving stock instance (ForallClauseX Show x, ForallExpX Show x, ForallPatX Show x, ForallStmtX Show x, Show (XId x)) => Show (Clause x)

instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x, Ord (XId x), ForallPatX Ord x, ForallStmtX Ord x) => Ord (Clause x) where
  (Clause _ ps1 _) `compare` (Clause _ ps2 _) = ps1 `compare` ps2

instance (Pretty (XId x)) => Pretty (Clause x) where
  pretty (Clause _ [] e) = sep (punctuate ";" $ NonEmpty.toList $ fmap pretty e)
  pretty (Clause _ ps e) = sep [sep (map (pprPatPrec 11) ps) <+> "->", sep (punctuate ";" $ NonEmpty.toList $ fmap pretty e)]

instance
  ForallClauseX U.WithUType x =>
  U.HasType (Clause x)
  where
  typeOf (Clause x _ _) = x ^. U.withUType

instance
  ForallClauseX S.WithType x =>
  S.HasType (Clause x)
  where
  typeOf (Clause x _ _) = x ^. S.withType

instance
  ( ForallExpX (U.HasUTerm S.TypeF U.TypeVar) x,
    ForallClauseX (U.HasUTerm S.TypeF U.TypeVar) x,
    ForallPatX (U.HasUTerm S.TypeF U.TypeVar) x
  ) =>
  U.HasUTerm S.TypeF U.TypeVar (Clause x)
  where
  walkOn f (Clause x ps e) = Clause <$> U.walkOn f x <*> traverse (U.walkOn f) ps <*> traverse (U.walkOn f) e

freevarsClause :: (Eq (XId x), Hashable (XId x)) => Clause x -> HashSet (XId x)
freevarsClause (Clause _ pats es) = HashSet.difference (foldMap freevarsStmt es) (mconcat (map bindVars pats))

-------------
-- Pattern --
-------------

data Pat x
  = VarP (XVarP x) (XId x)
  | ConP (XConP x) (XId x) [Pat x]
  | TupleP (XTupleP x) [Pat x]
  | RecordP (XRecordP x) [(WithPrefix (XId x), Pat x)]
  | ListP (XListP x) [Pat x]
  | UnboxedP (XUnboxedP x) (Literal Unboxed)

deriving stock instance (ForallPatX Eq x, Eq (XId x)) => Eq (Pat x)

deriving stock instance (ForallPatX Show x, Show (XId x)) => Show (Pat x)

deriving stock instance (ForallPatX Ord x, Ord (XId x)) => Ord (Pat x)

instance (Pretty (XId x)) => Pretty (Pat x) where
  pretty x = pprPatPrec 0 x

pprPatPrec :: (Pretty (XId x)) => Int -> Pat x -> Doc ann
pprPatPrec _ (VarP _ i) = pretty i
pprPatPrec _ (ConP _ i []) = pretty i
pprPatPrec d (ConP _ i ps) =
  maybeParens (d > 10) $ pretty i <+> sep (map (pprPatPrec 11) ps)
pprPatPrec _ (TupleP _ ps) =
  parens $ sep $ punctuate "," $ map pretty ps
pprPatPrec _ (RecordP _ kps) =
  braces $ sep $ punctuate "," $ map (\(k, p) -> pretty k <> ":" <+> pretty p) kps
pprPatPrec _ (ListP _ ps) =
  brackets $ sep $ punctuate "," $ map pretty ps
pprPatPrec _ (UnboxedP _ u) = pretty u

instance
  ForallPatX U.WithUType x =>
  U.HasType (Pat x)
  where
  typeOf (VarP x _) = x ^. U.withUType
  typeOf (ConP x _ _) = x ^. U.withUType
  typeOf (TupleP x _) = x ^. U.withUType
  typeOf (RecordP x _) = x ^. U.withUType
  typeOf (ListP x _) = x ^. U.withUType
  typeOf (UnboxedP x _) = x ^. U.withUType

instance
  ForallPatX S.WithType x =>
  S.HasType (Pat x)
  where
  typeOf (VarP x _) = x ^. S.withType
  typeOf (ConP x _ _) = x ^. S.withType
  typeOf (TupleP x _) = x ^. S.withType
  typeOf (RecordP x _) = x ^. S.withType
  typeOf (ListP x _) = x ^. S.withType
  typeOf (UnboxedP x _) = x ^. S.withType

instance
  ( ForallExpX (U.HasUTerm S.TypeF U.TypeVar) x,
    ForallClauseX (U.HasUTerm S.TypeF U.TypeVar) x,
    ForallPatX (U.HasUTerm S.TypeF U.TypeVar) x
  ) =>
  U.HasUTerm S.TypeF U.TypeVar (Pat x)
  where
  walkOn f = \case
    VarP x v -> VarP <$> U.walkOn f x <*> pure v
    ConP x c ps -> ConP <$> U.walkOn f x <*> pure c <*> traverse (U.walkOn f) ps
    TupleP x ps -> TupleP <$> U.walkOn f x <*> traverse (U.walkOn f) ps
    RecordP x kps -> RecordP <$> U.walkOn f x <*> traverse (bitraverse pure (U.walkOn f)) kps
    ListP x ps -> ListP <$> U.walkOn f x <*> traverse (U.walkOn f) ps
    UnboxedP x u -> UnboxedP <$> U.walkOn f x <*> U.walkOn f u

bindVars :: (Eq (XId x), Hashable (XId x)) => Pat x -> HashSet (XId x)
bindVars (VarP _ x) = HashSet.singleton x
bindVars (ConP _ _ ps) = mconcat $ map bindVars ps
bindVars (TupleP _ ps) = mconcat $ map bindVars ps
bindVars (RecordP _ kps) = mconcat $ map (bindVars . snd) kps
bindVars (ListP _ ps) = mconcat $ map bindVars ps
bindVars UnboxedP {} = mempty

makePrisms ''Pat

----------
-- Type --
----------

data Type x
  = TyApp (XTyApp x) (Type x) [Type x]
  | TyVar (XTyVar x) (XId x)
  | TyCon (XTyCon x) (XId x)
  | TyArr (XTyArr x) (Type x) (Type x)
  | TyTuple (XTyTuple x) [Type x]
  | TyRecord (XTyRecord x) [(XId x, Type x)]
  | TyLazy (XTyLazy x) (Type x)
  | TyDArr (XTyDArr x) (Type x) (Type x)

deriving stock instance (ForallTypeX Eq x, Eq (XId x)) => Eq (Type x)

deriving stock instance (ForallTypeX Show x, Show (XId x)) => Show (Type x)

instance (Pretty (XId x)) => Pretty (Type x) where
  pretty x = pprTypePrec 0 x

pprTypePrec :: (Pretty (XId x)) => Int -> Type x -> Doc ann
pprTypePrec d (TyApp _ t ts) =
  maybeParens (d > 11) $ pretty t <+> sep (map (pprTypePrec 12) ts)
pprTypePrec _ (TyVar _ i) = pretty i
pprTypePrec _ (TyCon _ i) = pretty i
pprTypePrec d (TyArr _ t1 t2) =
  maybeParens (d > 10) $ pprTypePrec 11 t1 <+> "->" <+> pprTypePrec 10 t2
pprTypePrec _ (TyTuple _ ts) = parens $ align $ sep $ punctuate "," $ map pretty ts
pprTypePrec _ (TyRecord _ kvs) = braces $ align $ sep $ punctuate "," $ map (\(k, v) -> pretty k <> ":" <+> pretty v) kvs
pprTypePrec _ (TyLazy _ t) = braces $ pretty t
pprTypePrec d (TyDArr _ t1 t2) =
  maybeParens (d > 10) $ pprTypePrec 11 t1 <+> "=>" <+> pprTypePrec 10 t2

getTyVars :: (Eq (XId x), Hashable (XId x)) => Type x -> HashSet (XId x)
getTyVars (TyApp _ t ts) = getTyVars t <> mconcat (map getTyVars ts)
getTyVars (TyVar _ v) = HashSet.singleton v
getTyVars TyCon {} = mempty
getTyVars (TyArr _ t1 t2) = getTyVars t1 <> getTyVars t2
getTyVars (TyTuple _ ts) = mconcat $ map getTyVars ts
getTyVars (TyRecord _ kvs) = mconcat $ map (getTyVars . snd) kvs
getTyVars (TyLazy _ t) = getTyVars t
getTyVars (TyDArr _ t1 t2) = getTyVars t1 <> getTyVars t2

-----------------
-- Declaration --
-----------------

data Decl x
  = ScDef (XScDef x) (XId x) (Exp x)
  | ScSig (XScSig x) (XId x) (Type x)
  | DataDef (XDataDef x) (XId x) [XId x] [(XId x, [Type x])]
  | TypeSynonym (XTypeSynonym x) (XId x) [XId x] (Type x)
  | Infix (XInfix x) Assoc Int (XId x)
  | Foreign (XForeign x) (XId x) (Type x)
  | Import (XImport x) ModuleName ImportList

deriving stock instance (ForallDeclX Eq x, Eq (XId x)) => Eq (Decl x)

deriving stock instance (ForallDeclX Show x, Show (XId x)) => Show (Decl x)

instance (Pretty (XId x)) => Pretty (Decl x) where
  pretty (ScDef _ f e) = sep [pretty f <+> "=", nest 2 $ pretty e]
  pretty (ScSig _ f t) = pretty f <+> ":" <+> pretty t
  pretty (DataDef _ d xs cs) =
    sep
      [ "data" <+> pretty d <+> sep (map pretty xs) <+> "=",
        nest 2 $ foldl1 (\a b -> sep [a, "|" <+> b]) $ map pprConDef cs
      ]
    where
      pprConDef (con, ts) = pretty con <+> sep (map (pprTypePrec 12) ts)
  pretty (TypeSynonym _ t xs t') =
    sep
      [ "type" <+> pretty t <+> sep (map pretty xs) <+> "=",
        pretty t'
      ]
  pretty (Infix _ a o x) = "infix" <> pretty a <+> pretty o <+> pretty x
  pretty (Foreign _ x t) = "foreign import" <+> pretty x <+> ":" <+> pretty t
  pretty (Import _ name All) = "module" <+> braces ".." <+> "=" <+> "import" <+> pretty name
  pretty (Import _ name (Selected xs)) = "module" <+> braces (sep $ punctuate "," $ map pretty xs) <+> "=" <+> "import" <+> pretty name
  pretty (Import _ name (As name')) = "module" <+> pretty name' <+> "=" <+> "import" <+> pretty name

makePrisms ''Decl

------------
-- Module --
------------

data Module x = Module {_moduleName :: ModuleName, _moduleDefinition :: XModule x}

deriving stock instance (ForallDeclX Eq x, Eq (XId x), Eq (XModule x)) => Eq (Module x)

deriving stock instance (ForallDeclX Show x, Show (XId x), Show (XModule x)) => Show (Module x)

instance (Pretty (XId x), Pretty (XModule x)) => Pretty (Module x) where
  pretty (Module name defs) =
    "module" <+> pretty name <+> "=" <> softline <> braces (pretty defs)

newtype ParsedDefinitions = ParsedDefinitions [Decl (Malgo 'Parse)]
  deriving stock (Eq, Show)

instance Pretty ParsedDefinitions where
  pretty (ParsedDefinitions ds) = sep $ map (\x -> pretty x <> ";") ds

-- モジュールの循環参照を防ぐため、このモジュールでtype instanceを定義する
type instance XModule (Malgo 'Parse) = ParsedDefinitions

type instance XModule (Malgo 'Rename) = BindGroup (Malgo 'Rename)

type instance XModule (Malgo 'TypeCheck) = BindGroup (Malgo 'TypeCheck)

type instance XModule (Malgo 'Refine) = BindGroup (Malgo 'Refine)

----------------
-- Bind group --
----------------

data BindGroup x = BindGroup
  { -- | 相互再帰的なグループに分割
    _scDefs :: [[ScDef x]],
    _scSigs :: [ScSig x],
    _dataDefs :: [DataDef x],
    _typeSynonyms :: [TypeSynonym x],
    _foreigns :: [Foreign x],
    _imports :: [Import x]
  }

type ScDef x = (XScDef x, XId x, Exp x)

type ScSig x = (XScSig x, XId x, Type x)

type DataDef x = (XDataDef x, XId x, [XId x], [(XId x, [Type x])])

type TypeSynonym x = (XTypeSynonym x, XId x, [XId x], Type x)

type Foreign x = (XForeign x, XId x, Type x)

type Import x = (XImport x, ModuleName, ImportList)

makeLenses ''BindGroup

deriving stock instance (ForallDeclX Eq x, Eq (XId x)) => Eq (BindGroup x)

deriving stock instance (ForallDeclX Show x, Show (XId x)) => Show (BindGroup x)

instance (Pretty (XId x)) => Pretty (BindGroup x) where
  pretty BindGroup {_scDefs, _scSigs, _dataDefs, _foreigns} =
    sep $
      punctuate ";" $
        map prettyDataDef _dataDefs
          <> map prettyForeign _foreigns
          <> map prettyScSig _scSigs
          <> concatMap (map prettyScDef) _scDefs
    where
      prettyDataDef (_, d, xs, cs) =
        sep
          [ "data" <+> pretty d <+> sep (map pretty xs) <+> "=",
            nest 2 $ foldl1 (\a b -> sep [a, "|" <+> b]) $ map pprConDef cs
          ]
      pprConDef (con, ts) = pretty con <+> sep (map (pprTypePrec 12) ts)
      prettyForeign (_, x, t) = "foreign import" <+> pretty x <+> ":" <+> pretty t
      prettyScSig (_, f, t) = pretty f <+> ":" <+> pretty t
      prettyScDef (_, f, e) =
        sep [pretty f <+> "=", pretty e]

makeBindGroup :: (XId x ~ Id a, Eq a) => [Decl x] -> BindGroup x
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

adjacents :: (Eq a1, XId x ~ Id a1) => (a, XId x, Exp x) -> (XId x, Int, [Int])
adjacents (_, f, e) =
  (f, f ^. idUniq, map (view idUniq) $ toList $ HashSet.delete f (freevars e))

makeSCC :: (Eq a1, XId x ~ Id a1) => [(a, XId x, Exp x)] -> [[XId x]]
makeSCC ds = map flattenSCC $ stronglyConnComp adjacents'
  where
    vertices = map (view _2 . adjacents) ds
    adjacents' = map ((\(l, v, vs) -> (l, v, filter (`elem` vertices) vs)) . adjacents) ds
