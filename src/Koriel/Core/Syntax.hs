{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- malgoの共通中間表現。
-- A正規形に近い。
module Koriel.Core.Syntax
  ( Unboxed (..),
    Atom (..),
    Obj (..),
    LocalDef (..),
    HasObject (..),
    HasVariable (..),
    Case (..),
    Exp (..),
    Program (..),
    HasAtom (..),
    runDef,
    let_,
    bind,
    cast,
    appObj,
    appCase,
    appProgram,
    freevars,
  )
where

import Control.Lens (Lens', Traversal', sans, traverseOf, traversed, _3, _4)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Generic.Data
import Koriel.Core.Op
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty
import Numeric (showHex)

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

class HasAtom f where
  atom :: Traversal' (f a) (Atom a)

instance HasAtom Atom where
  atom = identity

-- | heap objects
data Obj a
  = -- | function (arity >= 1)
    Fun [a] (Exp a)
  | -- | saturated constructor (arity >= 0)
    Pack Type Con [Atom a]
  | -- | record
    Record (HashMap Text (Atom a))
  deriving stock (Eq, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Binary, ToJSON, FromJSON)

instance HasType a => HasType (Obj a) where
  typeOf (Fun xs e) = map typeOf xs :-> typeOf e
  typeOf (Pack t _ _) = t
  typeOf (Record kvs) = RecordT (fmap typeOf kvs)

instance (Pretty a) => Pretty (Obj a) where
  pPrint (Fun xs e) = parens $ sep ["fun" <+> parens (sep $ map pPrint xs), pPrint e]
  pPrint (Pack _ c xs) = parens $ sep (["pack", pPrint c] <> map pPrint xs) -- The type of `pack` is already printed in the parent `LocalDef`.
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

data LocalDef a = LocalDef {_variable :: a, typ :: Type, _object :: Obj a}
  deriving stock (Eq, Show, Functor, Foldable, Generic, Data, Typeable)
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

instance (Pretty a) => Pretty (LocalDef a) where
  pPrint (LocalDef v t o) = parens $ pPrint v <+> pPrint t $$ pPrint o

-- | alternatives
data Case a
  = -- | constructor pattern
    Unpack Con [a] (Exp a)
  | -- | record pattern
    OpenRecord (HashMap Text a) (Exp a)
  | -- | unboxed value pattern
    Switch Unboxed (Exp a)
  | -- | variable pattern
    Bind a Type (Exp a)
  deriving stock (Eq, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Binary, ToJSON, FromJSON)

instance HasType a => HasType (Case a) where
  typeOf (Unpack _ _ e) = typeOf e
  typeOf (OpenRecord _ e) = typeOf e
  typeOf (Switch _ e) = typeOf e
  typeOf (Bind _ _ e) = typeOf e

instance (Pretty a) => Pretty (Case a) where
  pPrint (Unpack c xs e) =
    parens $ sep ["unpack" <+> parens (pPrint c <+> sep (map pPrint xs)), pPrint e]
  pPrint (OpenRecord pat e) =
    parens $ sep ["open", parens $ sep $ map (\(k, v) -> pPrint k <+> pPrint v) $ HashMap.toList pat, pPrint e]
  pPrint (Switch u e) = parens $ sep ["switch" <+> pPrint u, pPrint e]
  pPrint (Bind x t e) = parens $ sep ["bind", pPrint x, pPrint t, pPrint e]

instance HasFreeVar Case where
  freevars (Unpack _ xs e) = foldr sans (freevars e) xs
  freevars (OpenRecord pat e) = foldr sans (freevars e) (HashMap.elems pat)
  freevars (Switch _ e) = freevars e
  freevars (Bind x _ e) = sans x $ freevars e

instance HasAtom Case where
  atom f = \case
    Unpack con xs e -> Unpack con xs <$> traverseOf atom f e
    OpenRecord pat e -> OpenRecord pat <$> traverseOf atom f e
    Switch u e -> Switch u <$> traverseOf atom f e
    Bind a t e -> Bind a t <$> traverseOf atom f e

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
    --
    -- 'Cast'命令は、一見'Atom'の一種にできそうに見える。しかし、
    --
    -- * 'Atom'は再帰的にできない（'HasAtom'の制約）
    -- * 'Cast'は'Atom'上の操作（/=変数'a'上の操作）
    --
    -- よって、castをatomにすることは出来ない。
    Cast Type (Atom a)
  | -- | definition of local variables
    Let [LocalDef a] (Exp a)
  | -- | pattern matching
    Match (Exp a) [Case a]
  | -- | raise an internal error
    Error Type
  deriving stock (Eq, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Binary, ToJSON, FromJSON)

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
  typeOf (Match _ (c : _)) = typeOf c
  typeOf (Match _ []) = error "Match must have at least one case"
  typeOf (Error t) = t

instance (Pretty a) => Pretty (Exp a) where
  pPrint (Atom x) = pPrint x
  pPrint (Call f xs) = parens $ "call" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (CallDirect f xs) = parens $ "direct" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (RawCall p t xs) = parens $ "raw" <+> pPrint p <+> pPrint t <+> sep (map pPrint xs)
  pPrint (BinOp o x y) = parens $ "binop" <+> pPrint o <+> pPrint x <+> pPrint y
  pPrint (Cast ty x) = parens $ "cast" <+> pPrint ty <+> pPrint x
  pPrint (Let xs e) =
    parens $ "let" $$ parens (vcat (map pPrint xs)) $$ pPrint e
  pPrint (Match v cs) = parens $ "match" <+> pPrint v $$ vcat (toList $ fmap pPrint cs)
  pPrint (Error t) = parens $ "ERROR" <+> pPrint t

instance HasFreeVar Exp where
  freevars (Atom x) = freevars x
  freevars (Call f xs) = freevars f <> foldMap freevars xs
  freevars (CallDirect _ xs) = foldMap freevars xs
  freevars (RawCall _ _ xs) = foldMap freevars xs
  freevars (BinOp _ x y) = freevars x <> freevars y
  freevars (Cast _ x) = freevars x
  freevars (Let xs e) = foldr (sans . (._variable)) (freevars e <> foldMap (freevars . (._object)) xs) xs
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
    Let xs e -> Let <$> traverseOf (traversed . object . atom) f xs <*> traverseOf atom f e
    Match e cs -> Match <$> traverseOf atom f e <*> traverseOf (traversed . atom) f cs
    Error t -> pure (Error t)

-- | toplevel function definitions
data Program a = Program
  { topVars :: [(a, Type, Exp a)],
    topFuns :: [(a, [a], Type, Exp a)],
    extFuns :: [(Text, Type)]
  }
  deriving stock (Eq, Show, Functor, Generic)
  deriving anyclass (Binary, ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via Generically (Program a)

instance (Pretty a) => Pretty (Program a) where
  pPrint Program {..} =
    vcat $
      concat
        [ ["; variables"],
          map (\(v, t, e) -> parens $ sep ["define", pPrint v, pPrint t, pPrint e]) topVars,
          ["; functions"],
          map (\(f, ps, t, e) -> parens $ sep [sep ["define", parens (sep $ map pPrint $ f : ps), pPrint t], pPrint e]) topFuns,
          ["; externals"],
          map (\(f, t) -> parens $ sep ["extern", pPrint f, pPrint t]) extFuns
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
  Bind x t e -> Bind x t <$> f e

appProgram :: Traversal' (Program a) (Exp a)
appProgram f Program {..} =
  Program
    <$> traverseOf (traversed . _3) f topVars
    <*> traverseOf (traversed . _4) f topFuns
    <*> pure extFuns

newtype DefBuilderT m a = DefBuilderT {unDefBuilderT :: WriterT (Endo (Exp (Id Type))) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadTrans, MonadState s, MonadReader r)

runDef :: Functor f => DefBuilderT f (Exp (Id Type)) -> f (Exp (Id Type))
runDef m = uncurry (flip appEndo) <$> runWriterT (m.unDefBuilderT)

let_ :: (MonadIO m, MonadReader env m, HasUniqSupply env, HasModuleName env) => Type -> Obj (Id Type) -> DefBuilderT m (Atom (Id Type))
let_ otype obj = do
  x <- newTemporalId "let" otype
  DefBuilderT $ tell $ Endo $ \e -> Let [LocalDef x otype obj] e
  pure (Var x)

bind :: (MonadIO m, MonadReader env m, HasUniqSupply env, HasModuleName env) => Exp (Id Type) -> DefBuilderT m (Atom (Id Type))
bind (Atom a) = pure a
bind v = do
  x <- newTemporalId "d" (typeOf v)
  DefBuilderT $ tell $ Endo $ \e -> Match v [Bind x (typeOf x) e]
  pure (Var x)

cast :: (MonadIO m, MonadReader env m, HasUniqSupply env, HasModuleName env) => Type -> Exp (Id Type) -> DefBuilderT m (Atom (Id Type))
cast ty e
  | ty == typeOf e = bind e
  | otherwise = do
      v <- bind e
      x <- newTemporalId "cast" ty
      DefBuilderT $ tell $ Endo $ \e -> Match (Cast ty v) [Bind x ty e]
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
