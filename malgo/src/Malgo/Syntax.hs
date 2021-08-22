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
import qualified RIO.NonEmpty as NonEmpty

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

instance U.HasType (Literal x) where
  typeOf Int32 {} = U.TyPrim S.Int32T
  typeOf Int64 {} = U.TyPrim S.Int64T
  typeOf Float {} = U.TyPrim S.FloatT
  typeOf Double {} = U.TyPrim S.DoubleT
  typeOf Char {} = U.TyPrim S.CharT
  typeOf String {} = U.TyPrim S.StringT
  types f v = f (U.typeOf v) $> v

instance S.HasType (Literal x) where
  typeOf Int32 {} = S.TyPrim S.Int32T
  typeOf Int64 {} = S.TyPrim S.Int64T
  typeOf Float {} = S.TyPrim S.FloatT
  typeOf Double {} = S.TyPrim S.DoubleT
  typeOf Char {} = S.TyPrim S.CharT
  typeOf String {} = S.TyPrim S.StringT

toUnboxed :: Literal Boxed -> Literal Unboxed
toUnboxed = coerce

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
  pPrintPrec l d (TyApp _ t ts) =
    maybeParens (d > 11) $ pPrint t <+> sep (map (pPrintPrec l 12) ts)
  pPrintPrec _ _ (TyVar _ i) = pPrint i
  pPrintPrec _ _ (TyCon _ i) = pPrint i
  pPrintPrec l d (TyArr _ t1 t2) =
    maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TyTuple _ ts) = parens $ sep $ punctuate "," $ map pPrint ts
  pPrintPrec l _ (TyRecord _ kvs) = braces $ sep $ punctuate "," $ map (\(k, v) -> pPrintPrec l 0 k <> ":" <+> pPrintPrec l 0 v) kvs
  pPrintPrec _ _ (TyLazy _ t) = braces $ pPrint t
  pPrintPrec l d (TyDArr _ t1 t2) =
    maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "=>" <+> pPrintPrec l 10 t2

getTyVars :: (Eq (XId x), Hashable (XId x)) => Type x -> HashSet (XId x)
getTyVars (TyApp _ t ts) = getTyVars t <> mconcat (map getTyVars ts)
getTyVars (TyVar _ v) = HashSet.singleton v
getTyVars TyCon {} = mempty
getTyVars (TyArr _ t1 t2) = getTyVars t1 <> getTyVars t2
getTyVars (TyTuple _ ts) = mconcat $ map getTyVars ts
getTyVars (TyRecord _ kvs) = mconcat $ map (getTyVars . snd) kvs
getTyVars (TyLazy _ t) = getTyVars t
getTyVars (TyDArr _ t1 t2) = getTyVars t1 <> getTyVars t2

----------------
-- Expression --
----------------

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
          (NonEmpty.toList $ fmap (pPrintPrec l 0) cs)
  pPrintPrec l _ (Tuple _ xs) = parens $ sep $ punctuate "," $ map (pPrintPrec l 0) xs
  pPrintPrec l _ (Record _ kvs) = braces $ sep $ punctuate "," $ map (\(k, v) -> pPrintPrec l 0 k <> ":" <+> pPrintPrec l 0 v) kvs
  pPrintPrec l _ (List _ xs) = brackets $ sep $ punctuate "," $ map (pPrintPrec l 0) xs
  pPrintPrec l _ (Force _ x) = "!" <> pPrintPrec l 11 x
  pPrintPrec l _ (RecordAccess _ x) = "#" <> pPrintPrec l 0 x
  pPrintPrec _ _ (Ann _ e t) = parens $ pPrint e <+> ":" <+> pPrint t
  pPrintPrec _ _ (Seq _ ss) = parens $ sep $ punctuate ";" $ NonEmpty.toList $ fmap pPrint ss
  pPrintPrec _ _ (Parens _ x) = parens $ pPrint x

instance
  (ForallExpX U.HasType x, ForallClauseX U.HasType x, ForallPatX U.HasType x) =>
  U.HasType (Exp x)
  where
  typeOf (Var x _) = U.typeOf x
  typeOf (Unboxed x _) = U.typeOf x
  typeOf (Boxed x _) = U.typeOf x
  typeOf (Apply x _ _) = U.typeOf x
  typeOf (OpApp x _ _ _) = U.typeOf x
  typeOf (Fn x _) = U.typeOf x
  typeOf (Tuple x _) = U.typeOf x
  typeOf (Record x _) = U.typeOf x
  typeOf (List x _) = U.typeOf x
  typeOf (Force x _) = U.typeOf x
  typeOf (RecordAccess x _) = U.typeOf x
  typeOf (Ann x _ _) = U.typeOf x
  typeOf (Seq x _) = U.typeOf x
  typeOf (Parens x _) = U.typeOf x

  types f = \case
    Var x v -> Var <$> U.types f x <*> pure v
    Unboxed x u -> Unboxed <$> U.types f x <*> U.types f u
    Boxed x b -> Boxed <$> U.types f x <*> U.types f b
    Apply x e1 e2 -> Apply <$> U.types f x <*> U.types f e1 <*> U.types f e2
    OpApp x op e1 e2 -> OpApp <$> U.types f x <*> pure op <*> U.types f e1 <*> U.types f e2
    Fn x cs -> Fn <$> U.types f x <*> traverse (U.types f) cs
    Tuple x es -> Tuple <$> U.types f x <*> traverse (U.types f) es
    Record x kvs -> Record <$> U.types f x <*> traverse (\(k, v) -> (k,) <$> U.types f v) kvs
    List x es -> List <$> U.types f x <*> traverse (U.types f) es
    Force x e -> Force <$> U.types f x <*> U.types f e
    RecordAccess x l -> RecordAccess <$> U.types f x <*> pure l
    Ann x e t -> Ann <$> U.types f x <*> U.types f e <*> pure t
    Seq x ss -> Seq <$> U.types f x <*> traverse (U.types f) ss
    Parens x e -> Parens <$> U.types f x <*> U.types f e

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
  typeOf (Ann x _ _) = x ^. S.withType
  typeOf (Seq x _) = x ^. S.withType
  typeOf (Parens x _) = x ^. S.withType

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
freevars (Ann _ e _) = freevars e
freevars (Seq _ ss) = mconcat $ NonEmpty.toList $ fmap freevarsStmt ss
freevars (Parens _ e) = freevars e

----------
-- Stmt --
----------
data Stmt x
  = Let (XLet x) (XId x) (Exp x)
  | NoBind (XNoBind x) (Exp x)

deriving stock instance (ForallClauseX Eq x, ForallPatX Eq x, ForallExpX Eq x, ForallStmtX Eq x, ForallTypeX Eq x, Eq (XId x)) => Eq (Stmt x)

deriving stock instance (ForallClauseX Show x, ForallPatX Show x, ForallExpX Show x, ForallStmtX Show x, ForallTypeX Show x, Show (XId x)) => Show (Stmt x)

instance Pretty (XId x) => Pretty (Stmt x) where
  pPrint (Let _ v e) = "let" <+> pPrint v <+> "=" <+> pPrint e
  pPrint (NoBind _ e) = pPrint e

instance
  (ForallExpX U.HasType x, ForallClauseX U.HasType x, ForallPatX U.HasType x) =>
  U.HasType (Stmt x)
  where
  typeOf (Let _ _ e) = U.typeOf e
  typeOf (NoBind _ e) = U.typeOf e

  types f = \case
    Let x v e -> Let x v <$> U.types f e
    NoBind x e -> NoBind x <$> U.types f e

instance
  ForallExpX S.WithType x =>
  S.HasType (Stmt x)
  where
  typeOf (Let _ _ e) = S.typeOf e
  typeOf (NoBind _ e) = S.typeOf e

freevarsStmt :: (Eq (XId x), Hashable (XId x)) => Stmt x -> HashSet (XId x)
freevarsStmt (Let _ x e) = HashSet.delete x $ freevars e
freevarsStmt (NoBind _ e) = freevars e

------------
-- Clause --
------------

data Clause x = Clause (XClause x) [Pat x] (Exp x)

deriving stock instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x, ForallStmtX Eq x, ForallTypeX Eq x, Eq (XId x)) => Eq (Clause x)

deriving stock instance (ForallClauseX Show x, ForallExpX Show x, ForallPatX Show x, ForallStmtX Show x, ForallTypeX Show x, Show (XId x)) => Show (Clause x)

instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x, Ord (XId x), ForallPatX Ord x, ForallStmtX Ord x, ForallTypeX Ord x) => Ord (Clause x) where
  (Clause _ ps1 _) `compare` (Clause _ ps2 _) = ps1 `compare` ps2

instance (Pretty (XId x)) => Pretty (Clause x) where
  pPrintPrec _ _ (Clause _ [] e) = pPrint e
  pPrintPrec l _ (Clause _ ps e) = sep [sep (map (pPrintPrec l 11) ps) <+> "->", pPrint e]

instance
  (ForallClauseX U.HasType x, ForallPatX U.HasType x, ForallExpX U.HasType x) =>
  U.HasType (Clause x)
  where
  typeOf (Clause x _ _) = U.typeOf x

  types f (Clause x ps e) = Clause <$> U.types f x <*> traverse (U.types f) ps <*> U.types f e

instance
  ForallClauseX S.WithType x =>
  S.HasType (Clause x)
  where
  typeOf (Clause x _ _) = x ^. S.withType

freevarsClause :: (Eq (XId x), Hashable (XId x)) => Clause x -> HashSet (XId x)
freevarsClause (Clause _ pats e) = HashSet.difference (freevars e) (mconcat (map bindVars pats))

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

instance
  ForallPatX U.HasType x =>
  U.HasType (Pat x)
  where
  typeOf (VarP x _) = U.typeOf x
  typeOf (ConP x _ _) = U.typeOf x
  typeOf (TupleP x _) = U.typeOf x
  typeOf (RecordP x _) = U.typeOf x
  typeOf (ListP x _) = U.typeOf x
  typeOf (UnboxedP x _) = U.typeOf x

  types f = \case
    VarP x v -> VarP <$> U.types f x <*> pure v
    ConP x c ps -> ConP <$> U.types f x <*> pure c <*> traverse (U.types f) ps
    TupleP x ps -> TupleP <$> U.types f x <*> traverse (U.types f) ps
    RecordP x kps -> RecordP <$> U.types f x <*> traverse (bitraverse pure (U.types f)) kps
    ListP x ps -> ListP <$> U.types f x <*> traverse (U.types f) ps
    UnboxedP x u -> UnboxedP <$> U.types f x <*> U.types f u

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

bindVars :: (Eq (XId x), Hashable (XId x)) => Pat x -> HashSet (XId x)
bindVars (VarP _ x) = HashSet.singleton x
bindVars (ConP _ _ ps) = mconcat $ map bindVars ps
bindVars (TupleP _ ps) = mconcat $ map bindVars ps
bindVars (RecordP _ kps) = mconcat $ map (bindVars . snd) kps
bindVars (ListP _ ps) = mconcat $ map bindVars ps
bindVars UnboxedP {} = mempty

makePrisms ''Pat

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
  | Class (XClass x) (XId x) [XId x] (Type x)
  | Impl (XImpl x) (XId x) (Type x) (Exp x)

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
      pprConDef (con, ts) = pPrint con <+> sep (map (pPrintPrec prettyNormal 12) ts)
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
  pPrint (Class _ name params synType) = "class" <+> pPrint name <+> sep (map pPrint params) <+> "=" <+> pPrint synType
  pPrint (Impl _ name typ expr) = "impl" <+> pPrint name <+> ":" <+> pPrint typ <+> "=" <+> pPrint expr

makePrisms ''Decl

------------
-- Module --
------------

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
    _imports :: [Import x],
    _classes :: [Class x],
    _impls :: [Impl x]
  }

type ScDef x = (XScDef x, XId x, Exp x)

type ScSig x = (XScSig x, XId x, Type x)

type DataDef x = (XDataDef x, XId x, [XId x], [(XId x, [Type x])])

type TypeSynonym x = (XTypeSynonym x, XId x, [XId x], Type x)

type Foreign x = (XForeign x, XId x, Type x)

type Import x = (XImport x, ModuleName, ImportList)

type Class x = (XClass x, XId x, [XId x], Type x)

type Impl x = (XImpl x, XId x, Type x, Exp x)

makeLenses ''BindGroup

deriving stock instance (ForallDeclX Eq x, Eq (XId x)) => Eq (BindGroup x)

deriving stock instance (ForallDeclX Show x, Show (XId x)) => Show (BindGroup x)

instance (Pretty (XId x)) => Pretty (BindGroup x) where
  pPrint BindGroup {..} =
    sep $
      punctuate ";" $
        map prettyDataDef _dataDefs
          <> map prettyForeign _foreigns
          <> map prettyClass _classes
          <> map prettyImpl _impls
          <> map prettyScSig _scSigs
          <> concatMap (map prettyScDef) _scDefs
    where
      prettyDataDef (_, d, xs, cs) =
        sep
          [ "data" <+> pPrint d <+> sep (map pPrint xs) <+> "=",
            nest 2 $ foldl1 (\a b -> sep [a, "|" <+> b]) $ map pprConDef cs
          ]
      pprConDef (con, ts) = pPrint con <+> sep (map (pPrintPrec prettyNormal 12) ts)
      prettyForeign (_, x, t) = "foreign import" <+> pPrint x <+> ":" <+> pPrint t
      prettyScSig (_, f, t) = pPrint f <+> ":" <+> pPrint t
      prettyScDef (_, f, e) =
        sep [pPrint f <+> "=", pPrint e]
      prettyClass (_, name, params, synType) =
        "class" <+> pPrint name <+> sep (map pPrint params) <+> "=" <+> pPrint synType
      prettyImpl (_, name, synType, expr) =
        "impl" <+> pPrint name <+> ":" <+> pPrint synType <+> "=" <+> pPrint expr

makeBindGroup :: (XId x ~ Id a, Eq a) => [Decl x] -> BindGroup x
makeBindGroup ds =
  BindGroup
    { _scDefs = splitScDef (makeSCC $ mapMaybe scDef ds) (mapMaybe scDef ds),
      _scSigs = mapMaybe scSig ds,
      _dataDefs = mapMaybe dataDef ds,
      _typeSynonyms = mapMaybe typeSynonym ds,
      _foreigns = mapMaybe foreignDef ds,
      _imports = mapMaybe importDef ds,
      _classes = mapMaybe classDef ds,
      _impls = mapMaybe implDef ds
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
    classDef (Class x n ps ms) = Just (x, n, ps, ms)
    classDef _ = Nothing
    implDef (Impl x n t ms) = Just (x, n, t, ms)
    implDef _ = Nothing
    splitScDef sccs ds = map (mapMaybe (\n -> find (\d -> n == d ^. _2) ds)) sccs

adjacents :: (Eq a1, XId x ~ Id a1) => (a, XId x, Exp x) -> (XId x, Int, [Int])
adjacents (_, f, e) =
  (f, f ^. idUniq, map (view idUniq) $ toList $ HashSet.delete f (freevars e))

makeSCC :: (Eq a1, XId x ~ Id a1) => [(a, XId x, Exp x)] -> [[XId x]]
makeSCC ds = map flattenSCC $ stronglyConnComp adjacents'
  where
    vertices = map (view _2 . adjacents) ds
    adjacents' = map ((\(l, v, vs) -> (l, v, filter (`elem` vertices) vs)) . adjacents) ds
