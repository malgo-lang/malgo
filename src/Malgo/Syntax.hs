{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Syntax where

import Control.Lens (makeLenses, makePrisms, view, (^.), _2)
import Data.Foldable (foldl1)
import Data.Graph (flattenSCC, stronglyConnComp)
import Data.HashSet qualified as HashSet
import Koriel.Id
import Koriel.Pretty
import Language.LSP.Types.Lens (HasRange (range))
import Malgo.Infer.TypeRep hiding (TyApp, TyArr, TyCon, TyRecord, TyTuple, TyVar, Type, freevars)
import Malgo.Prelude
import Malgo.Syntax.Extension

-- | Unboxed and boxed literal
data Literal x = Int32 Int32 | Int64 Int64 | Float Float | Double Double | Char Char | String Text
  deriving stock (Show, Eq, Ord)

instance Pretty (Literal x) where
  pPrint (Int32 i) = pPrint (toInteger i)
  pPrint (Int64 i) = pPrint (toInteger i) <> "L"
  pPrint (Float f) = pPrint f <> "F"
  pPrint (Double d) = pPrint d
  pPrint (Char c) = quotes (pPrint c)
  pPrint (String s) = doubleQuotes (pPrint s)

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
  pPrintPrec l d (TyApp _ t ts) =
    maybeParens (d > 11) $ pPrint t <+> sep (map (pPrintPrec l 12) ts)
  pPrintPrec _ _ (TyVar _ i) = pPrint i
  pPrintPrec _ _ (TyCon _ i) = pPrint i
  pPrintPrec l d (TyArr _ t1 t2) =
    maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TyTuple _ ts) = parens $ sep $ punctuate "," $ map pPrint ts
  pPrintPrec l _ (TyRecord _ kvs) = braces $ sep $ punctuate "," $ map (\(k, v) -> pPrintPrec l 0 k <> ":" <+> pPrintPrec l 0 v) kvs
  pPrintPrec _ _ (TyBlock _ t) = braces $ pPrint t

instance (HasRange (XTyApp x) r, HasRange (XTyVar x) r, HasRange (XTyCon x) r, HasRange (XTyArr x) r, HasRange (XTyTuple x) r, HasRange (XTyRecord x) r, HasRange (XTyBlock x) r) => HasRange (Type x) r where
  range f = \case
    TyApp x t ts -> range f x <&> \x -> TyApp x t ts
    TyVar x i -> range f x <&> \x -> TyVar x i
    TyCon x i -> range f x <&> \x -> TyCon x i
    TyArr x t1 t2 -> range f x <&> \x -> TyArr x t1 t2
    TyTuple x ts -> range f x <&> \x -> TyTuple x ts
    TyRecord x kvs -> range f x <&> \x -> TyRecord x kvs
    TyBlock x t -> range f x <&> \x -> TyBlock x t

getTyVars :: Hashable (XId x) => Type x -> HashSet (XId x)
getTyVars (TyApp _ t ts) = getTyVars t <> mconcat (map getTyVars ts)
getTyVars (TyVar _ v) = one v
getTyVars TyCon {} = mempty
getTyVars (TyArr _ t1 t2) = getTyVars t1 <> getTyVars t2
getTyVars (TyTuple _ ts) = mconcat $ map getTyVars ts
getTyVars (TyRecord _ kvs) = mconcat $ map (getTyVars . snd) kvs
getTyVars (TyBlock _ t) = getTyVars t

-- * Expression

data Exp x
  = Var (XVar x) (XId x)
  | Unboxed (XUnboxed x) (Literal Unboxed)
  | Boxed (XBoxed x) (Literal Boxed)
  | Apply (XApply x) (Exp x) (Exp x)
  | OpApp (XOpApp x) (XId x) (Exp x) (Exp x)
  | Fn (XFn x) (NonEmpty (Clause x))
  | Tuple (XTuple x) [Exp x]
  | Record (XRecord x) [(Text, Exp x)]
  | List (XList x) [Exp x]
  | Ann (XAnn x) (Exp x) (Type x)
  | Seq (XSeq x) (NonEmpty (Stmt x))
  | Parens (XParens x) (Exp x)

deriving stock instance (ForallExpX Eq x, ForallClauseX Eq x, ForallPatX Eq x, ForallStmtX Eq x, ForallTypeX Eq x, Eq (XId x)) => Eq (Exp x)

deriving stock instance (ForallExpX Show x, ForallClauseX Show x, ForallPatX Show x, ForallStmtX Show x, ForallTypeX Show x, Show (XId x)) => Show (Exp x)

instance (Pretty (XId x)) => Pretty (Exp x) where
  pPrintPrec _ _ (Var _ i) = pPrint i
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
          (toList $ fmap (pPrintPrec l 0) cs)
  pPrintPrec l _ (Tuple _ xs) = parens $ sep $ punctuate "," $ map (pPrintPrec l 0) xs
  pPrintPrec l _ (Record _ kvs) = braces $ sep $ punctuate "," $ map (\(k, v) -> pPrintPrec l 0 k <> ":" <+> pPrintPrec l 0 v) kvs
  pPrintPrec l _ (List _ xs) = brackets $ sep $ punctuate "," $ map (pPrintPrec l 0) xs
  pPrintPrec _ _ (Ann _ e t) = parens $ pPrint e <+> ":" <+> pPrint t
  pPrintPrec _ _ (Seq _ ss) = parens $ sep $ punctuate ";" $ toList $ fmap pPrint ss
  pPrintPrec _ _ (Parens _ x) = parens $ pPrint x

instance
  (ForallExpX HasType x, ForallClauseX HasType x, ForallPatX HasType x) =>
  HasType (Exp x)
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

instance
  ( HasRange (XVar x) r,
    HasRange (XCon x) r,
    HasRange (XUnboxed x) r,
    HasRange (XBoxed x) r,
    HasRange (XApply x) r,
    HasRange (XOpApp x) r,
    HasRange (XFn x) r,
    HasRange (XTuple x) r,
    HasRange (XRecord x) r,
    HasRange (XList x) r,
    HasRange (XRecordAccess x) r,
    HasRange (XAnn x) r,
    HasRange (XSeq x) r,
    HasRange (XParens x) r
  ) =>
  HasRange (Exp x) r
  where
  range f = \case
    Var x v -> range f x <&> \x -> Var x v
    Unboxed x u -> range f x <&> \x -> Unboxed x u
    Boxed x b -> range f x <&> \x -> Boxed x b
    Apply x e1 e2 -> range f x <&> \x -> Apply x e1 e2
    OpApp x op e1 e2 -> range f x <&> \x -> OpApp x op e1 e2
    Fn x cs -> range f x <&> \x -> Fn x cs
    Tuple x es -> range f x <&> \x -> Tuple x es
    Record x kvs -> range f x <&> \x -> Record x kvs
    List x es -> range f x <&> \x -> List x es
    Ann x e t -> range f x <&> \x -> Ann x e t
    Seq x ss -> range f x <&> \x -> Seq x ss
    Parens x e -> range f x <&> \x -> Parens x e

freevars :: Hashable (XId x) => Exp x -> HashSet (XId x)
freevars (Var _ v) = one v
freevars (Unboxed _ _) = mempty
freevars (Boxed _ _) = mempty
freevars (Apply _ e1 e2) = freevars e1 <> freevars e2
freevars (OpApp _ op e1 e2) = HashSet.insert op $ freevars e1 <> freevars e2
freevars (Fn _ cs) = foldMap freevarsClause cs
  where
    freevarsClause :: Hashable (XId x) => Clause x -> HashSet (XId x)
    freevarsClause (Clause _ pats e) = HashSet.difference (freevars e) (mconcat (map bindVars pats))
    bindVars (VarP _ x) = one x
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
  = Let (XLet x) (XId x) (Exp x)
  | With (XWith x) (Maybe (XId x)) (Exp x)
  | NoBind (XNoBind x) (Exp x)

deriving stock instance (ForallClauseX Eq x, ForallPatX Eq x, ForallExpX Eq x, ForallStmtX Eq x, ForallTypeX Eq x, Eq (XId x)) => Eq (Stmt x)

deriving stock instance (ForallClauseX Show x, ForallPatX Show x, ForallExpX Show x, ForallStmtX Show x, ForallTypeX Show x, Show (XId x)) => Show (Stmt x)

instance Pretty (XId x) => Pretty (Stmt x) where
  pPrint (Let _ v e) = "let" <+> pPrint v <+> "=" <+> pPrint e
  pPrint (With _ Nothing e) = "with" <+> pPrint e
  pPrint (With _ (Just v) e) = "with" <+> pPrint v <+> "=" <+> pPrint e
  pPrint (NoBind _ e) = pPrint e

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

data Clause x = Clause (XClause x) [Pat x] (Exp x)

deriving stock instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x, ForallStmtX Eq x, ForallTypeX Eq x, Eq (XId x)) => Eq (Clause x)

deriving stock instance (ForallClauseX Show x, ForallExpX Show x, ForallPatX Show x, ForallStmtX Show x, ForallTypeX Show x, Show (XId x)) => Show (Clause x)

instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x, Ord (XId x), ForallPatX Ord x, ForallStmtX Ord x, ForallTypeX Ord x) => Ord (Clause x) where
  (Clause _ ps1 _) `compare` (Clause _ ps2 _) = ps1 `compare` ps2

instance (Pretty (XId x)) => Pretty (Clause x) where
  pPrintPrec _ _ (Clause _ [] e) = pPrint e
  pPrintPrec l _ (Clause _ ps e) = sep [sep (map (pPrintPrec l 11) ps) <+> "->", pPrint e]

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
  pPrintPrec _ _ (VarP _ i) = pPrint i
  pPrintPrec _ _ (ConP _ i []) = pPrint i
  pPrintPrec l d (ConP _ i ps) =
    maybeParens (d > 10) $ pPrint i <+> sep (map (pPrintPrec l 11) ps)
  pPrintPrec _ _ (TupleP _ ps) =
    parens $ sep $ punctuate "," $ map pPrint ps
  pPrintPrec l _ (RecordP _ kps) =
    braces $ sep $ punctuate "," $ map (\(k, p) -> pPrintPrec l 0 k <> ":" <+> pPrintPrec l 0 p) kps
  pPrintPrec _ _ (ListP _ ps) =
    brackets $ sep $ punctuate "," $ map pPrint ps
  pPrintPrec _ _ (UnboxedP _ u) = pPrint u
  pPrintPrec _ _ (BoxedP _ x) = pPrint x

instance
  ForallPatX HasType x =>
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
  = ScDef (XScDef x) (XId x) (Exp x)
  | ScSig (XScSig x) (XId x) (Type x)
  | DataDef (XDataDef x) (XId x) [(Range, XId x)] [(Range, XId x, [Type x])]
  | TypeSynonym (XTypeSynonym x) (XId x) [XId x] (Type x)
  | Infix (XInfix x) Assoc Int (XId x)
  | Foreign (XForeign x) (XId x) (Type x)
  | Import (XImport x) ModuleName ImportList

deriving stock instance (ForallDeclX Eq x, Eq (XId x)) => Eq (Decl x)

deriving stock instance (ForallDeclX Show x, Show (XId x)) => Show (Decl x)

instance (Pretty (XId x)) => Pretty (Decl x) where
  pPrint (ScDef _ f e) = sep [pPrint f <+> "=", nest 2 $ pPrint e]
  pPrint (ScSig _ f t) = pPrint f <+> ":" <+> pPrint t
  pPrint (DataDef _ d xs cs) =
    sep
      [ "data" <+> pPrint d <+> sep (map pPrint xs) <+> "=",
        nest 2 $ foldl1 (\a b -> sep [a, "|" <+> b]) $ map pprConDef cs
      ]
    where
      pprConDef (_, con, ts) = pPrint con <+> sep (map (pPrintPrec prettyNormal 12) ts)
  pPrint (TypeSynonym _ t xs t') =
    sep
      [ "type" <+> pPrint t <+> sep (map pPrint xs) <+> "=",
        pPrint t'
      ]
  pPrint (Infix _ a o x) = "infix" <> pPrint a <+> pPrint o <+> pPrint x
  pPrint (Foreign _ x t) = "foreign import" <+> pPrint x <+> ":" <+> pPrint t
  pPrint (Import _ name All) = "module" <+> braces ".." <+> "=" <+> "import" <+> pPrint name
  pPrint (Import _ name (Selected xs)) = "module" <+> braces (sep $ punctuate "," $ map pPrint xs) <+> "=" <+> "import" <+> pPrint name
  pPrint (Import _ name (As name')) = "module" <+> pPrint name' <+> "=" <+> "import" <+> pPrint name

makePrisms ''Decl

-- * Module

data Module x = Module {_moduleName :: ModuleName, _moduleDefinition :: XModule x}

deriving stock instance (ForallDeclX Eq x, Eq (XId x), Eq (XModule x)) => Eq (Module x)

deriving stock instance (ForallDeclX Show x, Show (XId x), Show (XModule x)) => Show (Module x)

instance (Pretty (XId x), Pretty (XModule x)) => Pretty (Module x) where
  pPrint (Module name defs) =
    "module" <+> pPrint name <+> "=" $+$ braces (pPrint defs)

newtype ParsedDefinitions = ParsedDefinitions [Decl (Malgo 'Parse)]
  deriving stock (Eq, Show)

instance Pretty ParsedDefinitions where
  pPrint (ParsedDefinitions ds) = sep $ map (\x -> pPrint x <> ";") ds

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

type ScDef x = (XScDef x, XId x, Exp x)

type ScSig x = (XScSig x, XId x, Type x)

type DataDef x = (XDataDef x, XId x, [(Range, XId x)], [(Range, XId x, [Type x])])

type TypeSynonym x = (XTypeSynonym x, XId x, [XId x], Type x)

type Foreign x = (XForeign x, XId x, Type x)

type Import x = (XImport x, ModuleName, ImportList)

makeLenses ''BindGroup

deriving stock instance (ForallDeclX Eq x, Eq (XId x)) => Eq (BindGroup x)

deriving stock instance (ForallDeclX Show x, Show (XId x)) => Show (BindGroup x)

instance (Pretty (XId x)) => Pretty (BindGroup x) where
  pPrint BindGroup {..} =
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
      pprConDef (_, con, ts) = pPrint con <+> sep (map (pPrintPrec prettyNormal 12) ts)
      prettyForeign (_, x, t) = "foreign import" <+> pPrint x <+> ":" <+> pPrint t
      prettyScSig (_, f, t) = pPrint f <+> ":" <+> pPrint t
      prettyScDef (_, f, e) = sep [pPrint f <+> "=", nest 2 $ pPrint e]

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
