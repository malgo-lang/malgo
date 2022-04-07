{-# LANGUAGE UndecidableInstances #-}

-- |
-- malgoの共通中間表現。
-- A正規形に近い。
module Koriel.Core.Syntax where

import Control.Lens (Lens', Traversal', lens, sans, traverseOf, traversed, view, _2)
import qualified Data.HashSet as HashSet
import Koriel.Core.Op
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty

class HasFreeVar f where
  -- | free variables
  freevars :: (Eq a, Hashable a) => f a -> HashSet a

-- | unboxed values
data Unboxed
  = Int32 Integer
  | Int64 Integer
  | Float Float
  | Double Double
  | Char Char
  | String Text
  | Bool Bool
  deriving stock (Eq, Show)

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
  deriving stock (Eq, Show, Functor, Foldable)

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
  deriving stock (Eq, Show, Functor, Foldable)

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
  | -- | application of llvm function
    RawCall Text Type [Atom a]
  | -- | binary operation
    BinOp Op (Atom a) (Atom a)
  | -- | type casting
    Cast Type (Atom a)
  | -- | definition of local variables
    Let [LocalDef a] (Exp a)
  | -- | pattern matching
    Match [Exp a] (NonEmpty (Case a))
  | -- | raise an internal error
    Error Type
  deriving stock (Eq, Show, Functor, Foldable)

-- instance Data a => Plated (Exp a)

instance HasType a => HasType (Exp a) where
  typeOf (Atom x) = typeOf x
  typeOf (Call f xs) = case typeOf f of
    ps :-> r -> go ps (map typeOf xs) r
    _ -> errorDoc $ "Invalid type:" <+> quotes (pPrint $ typeOf f)
    where
      go [] [] v = v
      go (p : ps) (x : xs) v = replaceOf tyVar p x (go ps xs v)
      go _ _ _ = error "length ps == length xs"
  typeOf (CallDirect f xs) = case typeOf f of
    ps :-> r -> go ps (map typeOf xs) r
    _ -> error "typeOf f must be ps :-> r"
    where
      go [] [] v = v
      go (p : ps) (x : xs) v = replaceOf tyVar p x (go ps xs v)
      go _ _ _ = error "length ps == length xs"
  typeOf (RawCall _ t xs) = case t of
    ps :-> r -> go ps (map typeOf xs) r
    _ -> error "t must be ps :-> r"
    where
      go [] [] v = v
      go (p : ps) (x : xs) v = replaceOf tyVar p x (go ps xs v)
      go _ _ _ = error "length ps == length xs"
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
  pPrint (Call f xs) = parens $ sep $ pPrint f : map pPrint xs
  pPrint (CallDirect f xs) = parens $ sep $ "direct" : pPrint f : map pPrint xs
  pPrint (RawCall p t xs) = parens $ sep $ "raw" : pPrint p : pPrint t : map pPrint xs
  pPrint (BinOp o x y) = parens $ sep [pPrint o, pPrint x, pPrint y]
  pPrint (Cast ty x) = parens $ sep ["cast", pPrint ty, pPrint x]
  pPrint (Let xs e) =
    parens $ "let" $$ parens (vcat (map pPrint xs)) $$ pPrint e
  pPrint (Match v cs) = parens $ sep $ "match" : pPrint v : toList (fmap pPrint cs)
  pPrint (Error _) = "ERROR"

instance HasFreeVar Exp where
  freevars (Atom x) = freevars x
  freevars (Call f xs) = freevars f <> foldMap freevars xs
  freevars (CallDirect _ xs) = foldMap freevars xs
  -- freevars (ExtCall _ _ xs) = foldMap freevars xs
  freevars (RawCall _ _ xs) = foldMap freevars xs
  freevars (BinOp _ x y) = freevars x <> freevars y
  freevars (Cast _ x) = freevars x
  freevars (Let xs e) = foldr (sans . view localDefVar) (freevars e <> foldMap (freevars . view localDefObj) xs) xs
  freevars (Match es cs) = foldMap freevars es <> foldMap freevars cs
  freevars (Error _) = mempty

instance HasAtom Exp where
  atom f = \case
    Atom x -> Atom <$> f x
    Call x xs -> Call <$> f x <*> traverse f xs
    CallDirect x xs -> CallDirect x <$> traverse f xs
    -- ExtCall p t xs -> ExtCall p t <$> traverse f xs
    RawCall p t xs -> RawCall p t <$> traverse f xs
    BinOp o x y -> BinOp o <$> f x <*> f y
    Cast ty x -> Cast ty <$> f x
    Let xs e -> Let <$> traverseOf (traversed . localDefObj . atom) f xs <*> traverseOf atom f e
    Match es cs -> Match <$> traverseOf (traversed . atom) f es <*> traverseOf (traversed . atom) f cs
    Error t -> pure (Error t)

-- | alternatives
data Case a = Case [Pat a] (Exp a)
  deriving stock (Eq, Show, Functor, Foldable)

instance HasType a => HasType (Case a) where
  typeOf (Case _ e) = typeOf e

instance Pretty a => Pretty (Case a) where
  pPrint (Case p e) = parens $ sep [pPrint p, pPrint e]

instance HasFreeVar Case where
  freevars (Case p e) = HashSet.difference (freevars e) (foldMap freevars p)

instance HasAtom Case where
  atom f = \case
    Case p e -> Case p <$> traverseOf atom f e

data Pat a
  = -- | constructor pattern
    Unpack [Con] Con [Pat a]
  | -- | unboxed value pattern
    Switch Unboxed
  | -- | variable pattern
    Bind a
  deriving stock (Eq, Show, Functor, Foldable)

instance Pretty a => Pretty (Pat a) where
  pPrint (Unpack cs c xs) = parens $ sep $ "unpack" : pPrint c : "in" : pPrint cs : map pPrint xs
  pPrint (Switch u) = parens $ "switch" <+> pPrint u
  pPrint (Bind x) = parens $ "bind" <+> pPrint x

instance HasFreeVar Pat where
  freevars (Unpack _ _ xs) = mconcat $ map freevars xs
  freevars (Switch _) = mempty
  freevars (Bind x) = one x

instance HasType a => HasType (Pat a) where
  typeOf (Unpack cs _ _) = SumT cs
  typeOf (Switch u) = typeOf u
  typeOf (Bind a) = typeOf a

-- | heap objects
data Obj a
  = -- | function (arity >= 1)
    Fun [a] (Exp a)
  | -- | saturated constructor (arity >= 0)
    Pack [Con] Con [Atom a]
  deriving stock (Eq, Show, Functor, Foldable)

instance HasType a => HasType (Obj a) where
  typeOf (Fun xs e) = map typeOf xs :-> typeOf e
  typeOf (Pack cs _ _) = SumT cs

instance Pretty a => Pretty (Obj a) where
  pPrint (Fun xs e) = parens $ sep ["fun" <+> parens (sep $ map pPrint xs), pPrint e]
  pPrint (Pack cs c xs) = parens $ sep (["pack", pPrint cs, pPrint c] <> map pPrint xs)

instance HasFreeVar Obj where
  freevars (Fun as e) = foldr sans (freevars e) as
  freevars (Pack _ _ xs) = foldMap freevars xs

instance HasAtom Obj where
  atom f = \case
    Fun xs e -> Fun xs <$> traverseOf atom f e
    Pack cs con xs -> Pack cs con <$> traverseOf (traversed . atom) f xs

-- | toplevel function definitions
data Program a = Program
  { _moduleName :: ModuleName,
    _topVars :: [(a, Exp a)],
    _topFuncs :: [(a, ([a], Exp a))],
    _extFuncs :: [(Text, Type)]
  }
  deriving stock (Eq, Show, Functor)

moduleName :: Lens' (Program a) ModuleName
moduleName = lens _moduleName (\p x -> p {_moduleName = x})

topVars :: Lens' (Program a) [(a, Exp a)]
topVars = lens _topVars (\p x -> p {_topVars = x})

topFuncs :: Lens' (Program a) [(a, ([a], Exp a))]
topFuncs = lens _topFuncs (\p x -> p {_topFuncs = x})

extFuncs :: Lens' (Program a) [(Text, Type)]
extFuncs = lens _extFuncs (\p x -> p {_extFuncs = x})

instance (Pretty a, HasType a) => Pretty (Program a) where
  pPrint Program {..} =
    vcat $
      concat
        [ ["variables:"],
          map (\(v, e) -> parens $ sep ["define", pPrint v, pPrint e]) _topVars,
          ["functions:"],
          map
            ( \(f, (ps, e)) ->
                parens $
                  sep [sep ["define", pPrint f, parens (sep $ map (\p -> pPrint p <> ":" <> pPrint (typeOf p)) ps)], pPrint e]
            )
            _topFuncs,
          ["externals:"],
          map (\(f, t) -> parens $ sep ["extern", pPrint f, pPrint t]) _extFuncs
        ]

appObj :: Traversal' (Obj a) (Exp a)
appObj f = \case
  Fun ps e -> Fun ps <$> f e
  o -> pure o

appCase :: Traversal' (Case a) (Exp a)
appCase f = \case
  Case p e -> Case p <$> f e

appProgram :: Traversal' (Program a) (Exp a)
appProgram f Program {..} =
  Program _moduleName
    <$> traverseOf (traversed . _2) f _topVars
    <*> traverseOf (traversed . _2 . _2) f _topFuncs
    <*> pure _extFuncs

newtype DefBuilderT m a = DefBuilderT {unDefBuilderT :: WriterT (Endo (Exp (Id Type))) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadTrans, MonadState s, MonadReader r)

runDef :: Functor f => DefBuilderT f (Exp (Id Type)) -> f (Exp (Id Type))
runDef m = uncurry (flip appEndo) <$> runWriterT (unDefBuilderT m)

let_ :: (MonadIO m, MonadReader env m, HasUniqSupply env) => Type -> Obj (Id Type) -> DefBuilderT m (Atom (Id Type))
let_ otype obj = do
  x <- newInternalId "$let" otype
  DefBuilderT $ tell $ Endo $ \e -> Let [LocalDef x obj] e
  pure (Var x)

destruct :: (MonadIO m, MonadReader env m, HasUniqSupply env) => Exp (Id Type) -> [Con] -> Con -> DefBuilderT m [Atom (Id Type)]
destruct val cs con@(Con _ ts) = do
  vs <- traverse (newInternalId "$p") ts
  DefBuilderT $ tell $ Endo $ \e -> Match [val] (Case [Unpack cs con $ map Bind vs] e :| [])
  pure $ map Var vs

bind :: (MonadIO m, MonadReader env m, HasUniqSupply env) => Exp (Id Type) -> DefBuilderT m (Atom (Id Type))
bind (Atom a) = pure a
bind v = do
  x <- newInternalId "$d" (typeOf v)
  DefBuilderT $ tell $ Endo $ \e -> Match [v] (Case [Bind x] e :| [])
  pure (Var x)

cast :: (MonadIO m, MonadReader env m, HasUniqSupply env) => Type -> Exp (Id Type) -> DefBuilderT m (Atom (Id Type))
cast ty e
  | ty == typeOf e = bind e
  | otherwise = do
    v <- bind e
    x <- newInternalId "$cast" ty
    DefBuilderT $ tell $ Endo $ \e -> Match [Cast ty v] (Case [Bind x] e :| [])
    pure (Var x)

mainFunc :: (MonadIO m, MonadReader env m, HasUniqSupply env) => [ModuleName] -> Exp (Id Type) -> m (Id Type, ([Id Type], Exp (Id Type)))
mainFunc depList e = do
  mainFuncId <- newExternalId "main" ([] :-> Int32T) (ModuleName "Builtin")
  mainFuncBody <- runDef $ do
    _ <- bind $ RawCall "GC_init" ([] :-> VoidT) []
    traverse_
      ( \(ModuleName modName) ->
          bind $ RawCall ("koriel_load_" <> modName) ([] :-> VoidT) []
      )
      depList
    pure e
  pure (mainFuncId, ([], mainFuncBody))
