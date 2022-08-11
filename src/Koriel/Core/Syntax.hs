{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- malgoの共通中間表現。
-- A正規形に近い。
module Koriel.Core.Syntax where

import Control.Lens (Lens', Traversal', lens, sans, traverseOf, traversed, view, _2)
import Data.Data (Data)
import qualified Data.HashMap.Strict as HashMap
import Koriel.Core.Op
import Koriel.Core.Type
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty

class HasFreeVar f where
  -- | free variables
  freevars :: Hashable a => f a -> HashSet a

-- | unboxed values
data Unboxed
  = Int32 Integer
  | Int64 Integer
  | Float Float
  | Double Double
  | Char Char
  | String Text
  | Bool Bool
  deriving stock (Eq, Ord, Show, Generic, Data, Typeable)

instance HasType Unboxed where
  typeOf Int32 {} = Int32T
  typeOf Int64 {} = Int64T
  typeOf Float {} = FloatT
  typeOf Double {} = DoubleT
  typeOf Char {} = CharT
  typeOf String {} = StringT
  typeOf Bool {} = BoolT

instance Pretty Unboxed where
  pPrint (Int32 x) = pPrint x
  pPrint (Int64 x) = pPrint x
  pPrint (Float x) = pPrint x
  pPrint (Double x) = pPrint x
  pPrint (Char x) = quotes (pPrint x)
  pPrint (String x) = doubleQuotes (pPrint x)
  pPrint (Bool True) = "True#"
  pPrint (Bool False) = "False#"

-- | atoms
data Atom a
  = -- | variable
    Var a
  | -- | literal of unboxed values
    Unboxed Unboxed
  deriving stock (Eq, Show, Functor, Foldable, Generic, Data, Typeable)

instance HasType a => HasType (Atom a) where
  typeOf (Var x) = typeOf x
  typeOf (Unboxed x) = typeOf x

instance Pretty a => Pretty (Atom a) where
  pPrint (Var x) = pPrint x
  pPrint (Unboxed x) = pPrint x

instance HasFreeVar Atom where
  freevars (Var x) = one x
  freevars Unboxed {} = mempty

class HasAtom f where
  atom :: Traversal' (f a) (Atom a)

instance HasAtom Atom where
  atom = identity

data LocalDef a = LocalDef {_localDefVar :: a, _localDefObj :: Obj a}
  deriving stock (Eq, Show, Functor, Foldable, Generic, Data, Typeable)

instance Pretty a => Pretty (LocalDef a) where
  pPrint (LocalDef v o) = parens $ pPrint v $$ pPrint o

localDefVar :: Lens' (LocalDef a) a
localDefVar = lens _localDefVar (\l v -> l {_localDefVar = v})

localDefObj :: Lens' (LocalDef a) (Obj a)
localDefObj = lens _localDefObj (\l o -> l {_localDefObj = o})

-- | expressions
data Exp a
  = -- | atom (variables and literals)
    Atom (Atom a)
  | -- | application of closure
    Call (Atom a) [Atom a]
  | -- | application of function (not closure)
    CallDirect a [Atom a]
  | --  | -- | application of external function
    --   ExtCall Text Type [Atom a]

    -- | application of llvm function
    RawCall Text Type [Atom a]
  | -- | binary operation
    BinOp Op (Atom a) (Atom a)
  | -- | type casting
    Cast Type (Atom a)
  | -- | definition of local variables
    Let [LocalDef a] (Exp a)
  | -- | pattern matching
    Match (Exp a) (NonEmpty (Case a))
  | -- | raise an internal error
    Error Type
  deriving stock (Eq, Show, Functor, Foldable, Generic, Data, Typeable)

instance HasType a => HasType (Exp a) where
  typeOf (Atom x) = typeOf x
  typeOf (Call f xs) = case typeOf f of
    ps :-> r | map typeOf xs == ps -> r
    _ -> errorDoc $ "Invalid type:" <+> quotes (pPrint $ typeOf f)
  typeOf (CallDirect f xs) = case typeOf f of
    ps :-> r | map typeOf xs == ps -> r
    _ -> error "typeOf f must be ps :-> r"
  typeOf (RawCall _ t xs) = case t of
    ps :-> r | map typeOf xs == ps -> r
    _ -> error "t must be ps :-> r"
  typeOf (BinOp o x _) = case o of
    Add -> typeOf x
    Sub -> typeOf x
    Mul -> typeOf x
    Div -> typeOf x
    Mod -> typeOf x
    FAdd -> typeOf x
    FSub -> typeOf x
    FMul -> typeOf x
    FDiv -> typeOf x
    Eq -> boolT
    Neq -> boolT
    Lt -> boolT
    Gt -> boolT
    Le -> boolT
    Ge -> boolT
    And -> boolT
    Or -> boolT
    where
      boolT = BoolT
  typeOf (Cast ty _) = ty
  typeOf (Let _ e) = typeOf e
  typeOf (Match _ (c :| _)) = typeOf c
  typeOf (Error t) = t

instance Pretty a => Pretty (Exp a) where
  pPrint (Atom x) = pPrint x
  pPrint (Call f xs) = parens $ pPrint f <+> sep (map pPrint xs)
  pPrint (CallDirect f xs) = parens $ "direct" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (RawCall p t xs) = parens $ "raw" <+> pPrint p <+> pPrint t <+> sep (map pPrint xs)
  pPrint (BinOp o x y) = parens $ pPrint o <+> pPrint x <+> pPrint y
  pPrint (Cast ty x) = parens $ "cast" <+> pPrint ty <+> pPrint x
  pPrint (Let xs e) =
    parens $ "let" $$ parens (vcat (map pPrint xs)) $$ pPrint e
  pPrint (Match v cs) = parens $ "match" <+> pPrint v $$ vcat (toList $ fmap pPrint cs)
  pPrint (Error _) = "ERROR"

instance HasFreeVar Exp where
  freevars (Atom x) = freevars x
  freevars (Call f xs) = freevars f <> foldMap freevars xs
  freevars (CallDirect _ xs) = foldMap freevars xs
  freevars (RawCall _ _ xs) = foldMap freevars xs
  freevars (BinOp _ x y) = freevars x <> freevars y
  freevars (Cast _ x) = freevars x
  freevars (Let xs e) = foldr (sans . view localDefVar) (freevars e <> foldMap (freevars . view localDefObj) xs) xs
  freevars (Match e cs) = freevars e <> foldMap freevars cs
  freevars (Error _) = mempty

instance HasAtom Exp where
  atom f = \case
    Atom x -> Atom <$> f x
    Call x xs -> Call <$> f x <*> traverse f xs
    CallDirect x xs -> CallDirect x <$> traverse f xs
    RawCall p t xs -> RawCall p t <$> traverse f xs
    BinOp o x y -> BinOp o <$> f x <*> f y
    Cast ty x -> Cast ty <$> f x
    Let xs e -> Let <$> traverseOf (traversed . localDefObj . atom) f xs <*> traverseOf atom f e
    Match e cs -> Match <$> traverseOf atom f e <*> traverseOf (traversed . atom) f cs
    Error t -> pure (Error t)

-- | alternatives
data Case a
  = -- | constructor pattern
    Unpack Con [a] (Exp a)
  | -- | record pattern
    OpenRecord (HashMap Text a) (Exp a)
  | -- | unboxed value pattern
    Switch Unboxed (Exp a)
  | -- | variable pattern
    Bind a (Exp a)
  deriving stock (Eq, Show, Functor, Foldable, Generic, Data, Typeable)

instance HasType a => HasType (Case a) where
  typeOf (Unpack _ _ e) = typeOf e
  typeOf (OpenRecord _ e) = typeOf e
  typeOf (Switch _ e) = typeOf e
  typeOf (Bind _ e) = typeOf e

instance Pretty a => Pretty (Case a) where
  pPrint (Unpack c xs e) =
    parens $ sep ["unpack" <+> parens (pPrint c <+> sep (map pPrint xs)), pPrint e]
  pPrint (OpenRecord pat e) =
    parens $ sep ["open", pPrint $ HashMap.toList pat, pPrint e]
  pPrint (Switch u e) = parens $ sep ["switch" <+> pPrint u, pPrint e]
  pPrint (Bind x e) = parens $ sep ["bind" <+> pPrint x, pPrint e]

instance HasFreeVar Case where
  freevars (Unpack _ xs e) = foldr sans (freevars e) xs
  freevars (OpenRecord pat e) = foldr sans (freevars e) (HashMap.elems pat)
  freevars (Switch _ e) = freevars e
  freevars (Bind x e) = sans x $ freevars e

instance HasAtom Case where
  atom f = \case
    Unpack con xs e -> Unpack con xs <$> traverseOf atom f e
    OpenRecord pat e -> OpenRecord pat <$> traverseOf atom f e
    Switch u e -> Switch u <$> traverseOf atom f e
    Bind a e -> Bind a <$> traverseOf atom f e

-- | heap objects
data Obj a
  = -- | function (arity >= 1)
    Fun [a] (Exp a)
  | -- | saturated constructor (arity >= 0)
    Pack Type Con [Atom a]
  | -- | record
    Record (HashMap Text (Atom a))
  deriving stock (Eq, Show, Functor, Foldable, Generic, Data, Typeable)

instance HasType a => HasType (Obj a) where
  typeOf (Fun xs e) = map typeOf xs :-> typeOf e
  typeOf (Pack t _ _) = t
  typeOf (Record kvs) = RecordT (fmap typeOf kvs)

instance Pretty a => Pretty (Obj a) where
  pPrint (Fun xs e) = parens $ sep ["fun" <+> parens (sep $ map pPrint xs), pPrint e]
  pPrint (Pack ty c xs) = parens $ sep (["pack", pPrint ty, pPrint c] <> map pPrint xs)
  pPrint (Record kvs) = parens $ sep ["record" <+> parens (sep $ map (\(k, v) -> pPrint k <+> pPrint v) (HashMap.toList kvs))]

instance HasFreeVar Obj where
  freevars (Fun as e) = foldr sans (freevars e) as
  freevars (Pack _ _ xs) = foldMap freevars xs
  freevars (Record kvs) = foldMap freevars kvs

instance HasAtom Obj where
  atom f = \case
    Fun xs e -> Fun xs <$> traverseOf atom f e
    Pack ty con xs -> Pack ty con <$> traverseOf (traversed . atom) f xs
    Record kvs -> Record <$> traverseOf (traversed . atom) f kvs

-- | toplevel function definitions
data Program a = Program
  { _moduleName :: ModuleName,
    _topVars :: [(a, Exp a)],
    _topFuncs :: [(a, ([a], Exp a))],
    _extFuncs :: [(Text, Type)]
  }
  deriving stock (Eq, Show, Functor, Generic)

moduleName :: Lens' (Program a) ModuleName
moduleName = lens _moduleName (\p x -> p {_moduleName = x})

topVars :: Lens' (Program a) [(a, Exp a)]
topVars = lens _topVars (\p x -> p {_topVars = x})

topFuncs :: Lens' (Program a) [(a, ([a], Exp a))]
topFuncs = lens _topFuncs (\p x -> p {_topFuncs = x})

extFuncs :: Lens' (Program a) [(Text, Type)]
extFuncs = lens _extFuncs (\p x -> p {_extFuncs = x})

instance Pretty a => Pretty (Program a) where
  pPrint Program {..} =
    vcat $
      concat
        [ ["variables:"],
          map (\(v, e) -> parens $ sep ["define", pPrint v, pPrint e]) _topVars,
          ["functions:"],
          map (\(f, (ps, e)) -> parens $ sep [sep ["define", pPrint f, parens (sep $ map pPrint ps)], pPrint e]) _topFuncs,
          ["externals:"],
          map (\(f, t) -> parens $ sep ["extern", pPrint f, pPrint t]) _extFuncs
        ]

appObj :: Traversal' (Obj a) (Exp a)
appObj f = \case
  Fun ps e -> Fun ps <$> f e
  o -> pure o

appCase :: Traversal' (Case a) (Exp a)
appCase f = \case
  Unpack con ps e -> Unpack con ps <$> f e
  OpenRecord kvs e -> OpenRecord kvs <$> f e
  Switch u e -> Switch u <$> f e
  Bind x e -> Bind x <$> f e

appProgram :: Traversal' (Program a) (Exp a)
appProgram f Program {..} =
  Program _moduleName
    <$> traverseOf (traversed . _2) f _topVars
    <*> traverseOf (traversed . _2 . _2) f _topFuncs
    <*> pure _extFuncs

-- | Generate main function.
mainFunc :: (MonadIO m, MonadReader env m, HasUniqSupply env UniqSupply) => [ModuleName] -> Exp (Id Type) -> m (Id Type, ([Id Type], Exp (Id Type)))
mainFunc depList e = do
  -- `Builtin.main` are compiled as `main` in `Koriel.Core.CodeGen.toName`
  mainFuncId <- newNativeId "main" ([] :-> Int32T)
  mainFuncBody <- runDef do
    _ <- bind $ RawCall "GC_init" ([] :-> VoidT) []
    traverse_
      do \modName -> bind $ RawCall ("koriel_load_" <> raw modName) ([] :-> VoidT) []
      depList
    pure e
  pure (mainFuncId, ([], mainFuncBody))

newtype DefBuilderT m a = DefBuilderT {unDefBuilderT :: WriterT (Endo (Exp (Id Type))) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadTrans, MonadState s, MonadReader r)

runDef :: Functor f => DefBuilderT f (Exp (Id Type)) -> f (Exp (Id Type))
runDef m = uncurry (flip appEndo) <$> runWriterT (unDefBuilderT m)

let_ :: (MonadIO m, MonadReader env m, HasUniqSupply env UniqSupply) => Type -> Obj (Id Type) -> DefBuilderT m (Atom (Id Type))
let_ otype obj = do
  x <- newTemporalId "let" otype
  DefBuilderT $ tell $ Endo $ \e -> Let [LocalDef x obj] e
  pure (Var x)

bind :: (MonadIO m, MonadReader env m, HasUniqSupply env UniqSupply) => Exp (Id Type) -> DefBuilderT m (Atom (Id Type))
bind (Atom a) = pure a
bind v = do
  x <- newTemporalId "d" (typeOf v)
  DefBuilderT $ tell $ Endo $ \e -> Match v (Bind x e :| [])
  pure (Var x)

cast :: (MonadIO m, MonadReader env m, HasUniqSupply env UniqSupply) => Type -> Exp (Id Type) -> DefBuilderT m (Atom (Id Type))
cast ty e
  | ty == typeOf e = bind e
  | otherwise = do
    v <- bind e
    x <- newTemporalId "cast" ty
    DefBuilderT $ tell $ Endo $ \e -> Match (Cast ty v) (Bind x e :| [])
    pure (Var x)

-- `destruct` is convenient when treating types that have only one constructor.
-- For example, if we can write `let Foo x = v;` as the syntax sugar of `let x = v |> { Foo x -> x | _ -> error }`,
-- we can use `destruct` to support this syntax sugar.
-- But `let Foo x = v` style has some problem:
-- 1. Programmer must check whether the type can be treated that have only one constructor.
-- 2. There is more safe and convenenient way: `if let` in Rust.

-- destruct :: (MonadIO m, MonadReader env m, HasUniqSupply env UniqSupply) => Exp (Id Type) -> Con -> DefBuilderT m [Atom (Id Type)]
-- destruct val con@(Con _ ts) = do
--   vs <- traverse (newTemporalId "p") ts
--   DefBuilderT $ tell $ Endo $ \e -> Match val (Unpack con vs e :| [])
--   pure $ map Var vs
