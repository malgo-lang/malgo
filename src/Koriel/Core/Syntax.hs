{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | AST definitions for Koriel language
module Koriel.Core.Syntax (
  Unboxed (..),
  Atom (..),
  Obj (..),
  LocalDef (..),
  HasObject (..),
  HasVariable (..),
  Case (..),
  Expr (..),
  Program (..),
  HasAtom (..),
  HasExpr (..),
  runDef,
  let_,
  bind,
  cast,
  freevars,
  _Unpack,
  _OpenRecord,
  _Exact,
  _Bind,
)
where

import Control.Lens (Lens', Plated (..), Traversal', makePrisms, sans, traverseOf, traversed, _2, _3, _4)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Char (showLitChar)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.String.Conversions
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Generic.Data
import Koriel.Core.Op
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty
import Numeric (showHex)

-- | 'f' may have free variables
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
  deriving anyclass (Binary, ToJSON, FromJSON)

instance HasType Unboxed where
  typeOf Int32 {} = Int32T
  typeOf Int64 {} = Int64T
  typeOf Float {} = FloatT
  typeOf Double {} = DoubleT
  typeOf Char {} = CharT
  typeOf String {} = StringT
  typeOf Bool {} = BoolT

instance Pretty Unboxed where
  pPrint (Int32 x) = pPrint x <> "_i32"
  pPrint (Int64 x) = pPrint x <> "_i64"
  pPrint (Float x) = text (showHex (castFloatToWord32 x) "") <> "_f32" <+> "#|" <> pPrint x <> "|#"
  pPrint (Double x) = text (showHex (castDoubleToWord64 x) "") <> "_f64" <+> "#|" <> pPrint x <> "|#"
  pPrint (Char x) = quotes (text $ convertString $ showLitChar x "")
  pPrint (String x) = doubleQuotes (text $ concatMap (`showLitChar` "") $ convertString @_ @String x)
  pPrint (Bool True) = "True#"
  pPrint (Bool False) = "False#"

-- | atoms
data Atom a
  = -- | variable
    Var a
  | -- | literal of unboxed values
    Unboxed Unboxed
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Binary, ToJSON, FromJSON)

instance HasType a => HasType (Atom a) where
  typeOf (Var x) = typeOf x
  typeOf (Unboxed x) = typeOf x

instance Pretty a => Pretty (Atom a) where
  pPrint (Var x) = pPrint x
  pPrint (Unboxed x) = pPrint x

instance HasFreeVar Atom where
  freevars (Var x) = one x
  freevars Unboxed {} = mempty

-- | 'f' may include atoms
class HasAtom f where
  atom :: Traversal' (f a) (Atom a)

instance HasAtom Atom where
  atom = identity

-- | heap objects
data Obj a
  = -- | function (arity >= 1)
    Fun [a] (Expr a)
  | -- | saturated constructor (arity >= 0)
    Pack Type Con [Atom a]
  | -- | record
    Record (HashMap Text (Atom a))
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Binary, ToJSON, FromJSON)

instance HasType a => HasType (Obj a) where
  typeOf (Fun xs e) = map typeOf xs :-> typeOf e
  typeOf (Pack t _ _) = t
  typeOf (Record kvs) = RecordT (fmap typeOf kvs)

instance Pretty a => Pretty (Obj a) where
  pPrint (Fun xs e) = parens $ sep ["fun" <+> parens (sep $ map pPrint xs), pPrint e]
  pPrint (Pack ty c xs) = parens $ sep (["pack", pPrint ty, pPrint c] <> map pPrint xs)
  pPrint (Record kvs) =
    parens $
      sep
        [ "record"
            <+> parens
              ( sep $
                  map
                    ( \(k, v) ->
                        pPrint k
                          <+> pPrint v
                    )
                    (HashMap.toList kvs)
              )
        ]

instance HasFreeVar Obj where
  freevars (Fun as e) = foldr sans (freevars e) as
  freevars (Pack _ _ xs) = foldMap freevars xs
  freevars (Record kvs) = foldMap freevars kvs

instance HasAtom Obj where
  atom f = \case
    Fun xs e -> Fun xs <$> traverseOf atom f e
    Pack ty con xs -> Pack ty con <$> traverseOf (traversed . atom) f xs
    Record kvs -> Record <$> traverseOf (traversed . atom) f kvs

-- | Let bindings
data LocalDef a = LocalDef {_variable :: a, typ :: Type, _object :: Obj a}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Binary, ToJSON, FromJSON)

class HasObject s a | s -> a where
  object :: Lens' s a

instance HasObject (LocalDef a) (Obj a) where
  {-# INLINE object #-}
  object f (LocalDef x1 t x2) = fmap (LocalDef x1 t) (f x2)

class HasVariable s a | s -> a where
  variable :: Lens' s a

instance HasVariable (LocalDef a) a where
  {-# INLINE variable #-}
  variable f (LocalDef x1 t x2) = fmap (\x1 -> LocalDef x1 t x2) (f x1)

instance Pretty a => Pretty (LocalDef a) where
  pPrint (LocalDef v t o) = parens $ pPrint v <+> pPrint t $$ pPrint o

instance HasAtom LocalDef where
  atom = object . atom

-- | alternatives
data Case a
  = -- | constructor pattern
    Unpack Con [a] (Expr a)
  | -- | record pattern
    OpenRecord (HashMap Text a) (Expr a)
  | -- | unboxed value pattern
    Exact Unboxed (Expr a)
  | -- | variable pattern
    Bind a Type (Expr a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Binary, ToJSON, FromJSON)

instance HasType a => HasType (Case a) where
  typeOf (Unpack _ _ e) = typeOf e
  typeOf (OpenRecord _ e) = typeOf e
  typeOf (Exact _ e) = typeOf e
  typeOf (Bind _ _ e) = typeOf e

instance Pretty a => Pretty (Case a) where
  pPrint (Unpack c xs e) =
    parens $ sep ["unpack" <+> parens (pPrint c <+> sep (map pPrint xs)), pPrint e]
  pPrint (OpenRecord pat e) =
    parens $ sep ["open", parens $ sep $ map (\(k, v) -> pPrint k <+> pPrint v) $ HashMap.toList pat, pPrint e]
  pPrint (Exact u e) = parens $ sep ["exact" <+> pPrint u, pPrint e]
  pPrint (Bind x t e) = parens $ sep ["bind", pPrint x, pPrint t, pPrint e]

instance HasFreeVar Case where
  freevars (Unpack _ xs e) = foldr sans (freevars e) xs
  freevars (OpenRecord pat e) = foldr sans (freevars e) (HashMap.elems pat)
  freevars (Exact _ e) = freevars e
  freevars (Bind x _ e) = sans x $ freevars e

instance HasAtom Case where
  atom f = \case
    Unpack con xs e -> Unpack con xs <$> traverseOf atom f e
    OpenRecord pat e -> OpenRecord pat <$> traverseOf atom f e
    Exact u e -> Exact u <$> traverseOf atom f e
    Bind a t e -> Bind a t <$> traverseOf atom f e

-- | expressions
data Expr a
  = -- | atom (variables and literals)
    Atom (Atom a)
  | -- | application of closure
    Call (Atom a) [Atom a]
  | -- | application of function (not closure)
    CallDirect a [Atom a]
  | -- | application of primitive function
    -- Primitive functions are defined in target language.
    RawCall Text Type [Atom a]
  | -- | binary operation
    BinOp Op (Atom a) (Atom a)
  | -- | type casting
    --
    -- 'Cast'命令は、一見'Atom'の一種にできそうに見える。しかし、
    --
    -- * 'Atom'は再帰的にできない（'HasAtom'の制約）
    -- * 'Cast'は'Atom'上の操作（/=変数'a'上の操作）
    --
    -- よって、castをatomにすることは出来ない。
    Cast Type (Atom a)
  | -- | definition of local variables
    Let [LocalDef a] (Expr a)
  | -- | pattern matching
    Match (Expr a) [Case a]
  | -- | switch expression
    Switch
      (Atom a)
      [(Tag, Expr a)]
      -- ^ cases
      (Expr a)
      -- ^ default case
  | -- | switch by unboxed value
    SwitchUnboxed
      (Atom a)
      [(Unboxed, Expr a)]
      -- ^ cases
      (Expr a)
      -- ^ default case
  | -- | destruct a value
    Destruct (Atom a) Con [a] (Expr a)
  | -- | destruct a record
    DestructRecord (Atom a) (HashMap Text a) (Expr a)
  | -- | Assign a value to a variable
    Assign a (Expr a) (Expr a)
  | -- | raise an internal error
    Error Type
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Binary, ToJSON, FromJSON)

instance HasType a => HasType (Expr a) where
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
  typeOf (Match _ (c : _)) = typeOf c
  typeOf (Match _ []) = error "Match must have at least one case"
  typeOf (Switch _ ((_, e) : _) _) = typeOf e
  typeOf (Switch _ [] e) = typeOf e
  typeOf (SwitchUnboxed _ ((_, e) : _) _) = typeOf e
  typeOf (SwitchUnboxed _ [] e) = typeOf e
  typeOf (Destruct _ _ _ e) = typeOf e
  typeOf (DestructRecord _ _ e) = typeOf e
  typeOf (Assign _ _ e) = typeOf e
  typeOf (Error t) = t

instance Pretty a => Pretty (Expr a) where
  pPrint (Atom x) = pPrint x
  pPrint (Call f xs) = parens $ "call" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (CallDirect f xs) = parens $ "direct" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (RawCall p t xs) = parens $ "raw" <+> pPrint p <+> pPrint t <+> sep (map pPrint xs)
  pPrint (BinOp o x y) = parens $ "binop" <+> pPrint o <+> pPrint x <+> pPrint y
  pPrint (Cast ty x) = parens $ "cast" <+> pPrint ty <+> pPrint x
  pPrint (Let xs e) =
    parens $ "let" $$ parens (vcat (map pPrint xs)) $$ pPrint e
  pPrint (Match v cs) = parens $ "match" <+> pPrint v $$ vcat (toList $ fmap pPrint cs)
  pPrint (Switch v cs e) = parens $ "switch" <+> pPrint v $$ vcat (toList $ fmap pPrintCase cs) $$ parens ("default" <+> pPrint e)
    where
      pPrintCase (t, e) = parens $ pPrint t <+> pPrint e
  pPrint (SwitchUnboxed v cs e) = parens $ "switch-unboxed" <+> pPrint v $$ vcat (toList $ fmap pPrintCase cs) $$ parens ("default" <+> pPrint e)
    where
      pPrintCase (t, e) = parens $ pPrint t <+> pPrint e
  pPrint (Destruct v con xs e) = parens $ "destruct" <+> pPrint v <+> pPrint con <+> parens (sep (map pPrint xs)) $$ pPrint e
  pPrint (DestructRecord v kvs e) =
    parens $ "destruct-record" <+> pPrint v <+> parens (sep (map (\(k, v) -> pPrint k <+> pPrint v) $ HashMap.toList kvs)) $$ pPrint e
  pPrint (Assign x v e) = parens $ "=" <+> pPrint x <+> pPrint v $$ pPrint e
  pPrint (Error t) = parens $ "ERROR" <+> pPrint t

instance HasFreeVar Expr where
  freevars (Atom x) = freevars x
  freevars (Call f xs) = freevars f <> foldMap freevars xs
  freevars (CallDirect _ xs) = foldMap freevars xs
  freevars (RawCall _ _ xs) = foldMap freevars xs
  freevars (BinOp _ x y) = freevars x <> freevars y
  freevars (Cast _ x) = freevars x
  freevars (Let xs e) = foldr (sans . (._variable)) (freevars e <> foldMap (freevars . (._object)) xs) xs
  freevars (Match e cs) = freevars e <> foldMap freevars cs
  freevars (Switch v cs e) = freevars v <> foldMap (freevars . snd) cs <> freevars e
  freevars (SwitchUnboxed v cs e) = freevars v <> foldMap (freevars . snd) cs <> freevars e
  freevars (Destruct v _ xs e) =
    freevars v
      <> HashSet.difference
        (freevars e)
        (HashSet.fromList xs)
  freevars (DestructRecord v kvs e) =
    freevars v
      <> HashSet.difference
        (freevars e)
        (HashSet.fromList $ HashMap.elems kvs)
  freevars (Assign x v e) = freevars v <> sans x (freevars e)
  freevars (Error _) = mempty

instance HasAtom Expr where
  atom f = \case
    Atom x -> Atom <$> f x
    Call x xs -> Call <$> f x <*> traverse f xs
    CallDirect x xs -> CallDirect x <$> traverse f xs
    RawCall p t xs -> RawCall p t <$> traverse f xs
    BinOp o x y -> BinOp o <$> f x <*> f y
    Cast ty x -> Cast ty <$> f x
    Let xs e -> Let <$> traverseOf (traversed . atom) f xs <*> traverseOf atom f e
    Match e cs -> Match <$> traverseOf atom f e <*> traverseOf (traversed . atom) f cs
    Switch v cs e -> Switch <$> f v <*> traverseOf (traversed . _2 . atom) f cs <*> traverseOf atom f e
    SwitchUnboxed v cs e -> SwitchUnboxed <$> f v <*> traverseOf (traversed . _2 . atom) f cs <*> traverseOf atom f e
    Destruct v con xs e -> Destruct <$> f v <*> pure con <*> pure xs <*> traverseOf atom f e
    DestructRecord v kvs e -> DestructRecord <$> f v <*> pure kvs <*> traverseOf atom f e
    Assign x v e -> Assign x <$> traverseOf atom f v <*> traverseOf atom f e
    Error t -> pure (Error t)

class HasExpr f where
  expr :: Traversal' (f a) (Expr a)

instance HasExpr Expr where
  expr = identity

instance Plated (Expr a) where
  plate :: Traversal' (Expr a) (Expr a)
  plate _ e@Atom {} = pure e
  plate _ e@Call {} = pure e
  plate _ e@CallDirect {} = pure e
  plate _ e@RawCall {} = pure e
  plate _ e@BinOp {} = pure e
  plate _ e@Cast {} = pure e
  plate f (Let xs e) = Let <$> (traverseOf (traversed . expr) f xs) <*> f e
  plate f (Match e cs) = Match <$> f e <*> (traverseOf (traversed . expr) f cs)
  plate f (Switch v cs e) = Switch v <$> (traverseOf (traversed . _2) f cs) <*> f e
  plate f (SwitchUnboxed v cs e) = SwitchUnboxed v <$> (traverseOf (traversed . _2) f cs) <*> f e
  plate f (Destruct v con xs e) = Destruct v con xs <$> f e
  plate f (DestructRecord v kvs e) = DestructRecord v kvs <$> f e
  plate f (Assign x v e) = Assign x <$> f v <*> f e
  plate _ e@Error {} = pure e
  {-# INLINE plate #-}

-- | toplevel function definitions
data Program a = Program
  { topVars :: [(a, Type, Expr a)],
    topFuns :: [(a, [a], Type, Expr a)],
    extFuns :: [(Text, Type)]
  }
  deriving stock (Eq, Show, Functor, Generic)
  deriving anyclass (Binary, ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via Generically (Program a)

instance (Pretty a, Ord a) => Pretty (Program a) where
  pPrint Program {..} =
    vcat $
      concat
        [ ["; variables"],
          map (\(v, t, e) -> parens $ sep ["define" <+> pPrint v, pPrint t, pPrint e]) topVars,
          ["; functions"],
          map (\(f, ps, t, e) -> parens $ sep [sep ["define" <+> parens (sep $ map pPrint $ f : ps), pPrint t], pPrint e]) topFuns,
          ["; externals"],
          map (\(f, t) -> parens $ sep ["extern", "%" <> pPrint f, pPrint t]) extFuns
        ]

instance HasExpr Obj where
  expr f = \case
    Fun ps e -> Fun ps <$> f e
    o -> pure o

instance HasExpr LocalDef where
  expr = object . expr

instance HasExpr Case where
  expr f = \case
    Unpack con ps e -> Unpack con ps <$> f e
    OpenRecord kvs e -> OpenRecord kvs <$> f e
    Exact u e -> Exact u <$> f e
    Bind x t e -> Bind x t <$> f e

instance HasExpr Program where
  expr f Program {..} =
    Program
      <$> traverseOf (traversed . _3) f topVars
      <*> traverseOf (traversed . _4) f topFuns
      <*> pure extFuns

newtype DefBuilderT m a = DefBuilderT {unDefBuilderT :: WriterT (Endo (Expr (Id Type))) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState s, MonadReader r)

runDef :: Functor f => DefBuilderT f (Expr (Id Type)) -> f (Expr (Id Type))
runDef m = uncurry (flip appEndo) <$> runWriterT (m.unDefBuilderT)

let_ :: (MonadIO m, MonadReader env m, HasUniqSupply env, HasModuleName env) => Type -> Obj (Id Type) -> DefBuilderT m (Atom (Id Type))
let_ otype obj = do
  x <- newTemporalId "let" otype
  DefBuilderT $ tell $ Endo $ \e -> Let [LocalDef x otype obj] e
  pure (Var x)

bind :: (MonadIO m, MonadReader env m, HasUniqSupply env, HasModuleName env) => Expr (Id Type) -> DefBuilderT m (Atom (Id Type))
bind (Atom a) = pure a
bind v = do
  x <- newTemporalId "d" (typeOf v)
  DefBuilderT $ tell $ Endo $ \e ->
    Assign x v e
  pure (Var x)

cast :: (MonadIO m, MonadReader env m, HasUniqSupply env, HasModuleName env) => Type -> Expr (Id Type) -> DefBuilderT m (Atom (Id Type))
cast ty e
  | ty == typeOf e = bind e
  | otherwise = do
      v <- bind e
      x <- newTemporalId "cast" ty
      DefBuilderT $ tell $ Endo $ \e -> Assign x (Cast ty v) e
      pure (Var x)

-- `destruct` is convenient when treating types that have only one constructor.
-- For example, if we can write `let Foo x = v;` as the syntax sugar of `let x = v |> { Foo x -> x | _ -> error }`,
-- we can use `destruct` to support this syntax sugar.
-- But `let Foo x = v` style has some problem:
-- 1. Programmer must check whether the type can be treated that have only one constructor.
-- 2. There is more safe and convenenient way: `if let` in Rust.

-- destruct :: (MonadIO m, MonadReader env m, HasUniqSupply env UniqSupply) => Expr (Id Type) -> Con -> DefBuilderT m [Atom (Id Type)]
-- destruct val con@(Con _ ts) = do
--   vs <- traverse (newTemporalId "p") ts
--   DefBuilderT $ tell $ Endo $ \e -> Match val (Unpack con vs e :| [])
--   pure $ map Var vs

makePrisms ''Case
