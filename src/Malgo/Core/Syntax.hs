{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

-- | AST definitions for Core language
module Malgo.Core.Syntax
  ( Program (..),
    Expr (..),
    HasExpr (..),
    HasFreeVar (..),
    Atom (..),
    HasAtom (..),
    Unboxed (..),
    LocalDef (..),
    HasObject (..),
    HasVariable (..),
    Obj (..),
    Case (..),
    _Unpack,
    _OpenRecord,
    _Exact,
    _Bind,
    runDef,
    let_,
    bind,
    cast,
    callGraph,
    searchMain,
  )
where

import Control.Lens (Plated (..), Traversal', sans, traverseOf, traversed, _2, _3, _4)
import Control.Lens.Lens (Lens')
import Control.Lens.TH (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Graph
import Data.Map.Strict qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Data.Set qualified as Set
import Data.Store.TH
import Data.String.Conversions
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Effectful.Writer.Static.Local (Writer, runWriter, tell)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Generic.Data
import Malgo.Core.Type
import Malgo.Id
import Malgo.Module
import Malgo.MonadUniq
import Malgo.Prelude
import Malgo.SExpr qualified as S
import Numeric (showHex)
import Prettyprinter (dquotes, parens, sep, squotes, vcat, vsep, (<+>))

-- | toplevel function definitions
data Program a = Program
  { topVars :: [(a, Type, Expr a)],
    topFuns :: [(a, [a], Type, Expr a)],
    extFuns :: [(Text, Type)]
  }
  deriving stock (Eq, Show, Functor, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via Generically (Program a)

searchMain :: Program (Meta a) -> Maybe (Meta a)
searchMain Program {..} =
  let mains = map (\(a, _, _, _) -> a) $ filter (\(a, _, _, _) -> a.id.name == "main") topFuns
   in case mains of
        [] -> Nothing
        [m] -> Just m
        _ -> error "Multiple main functions"

instance (Pretty a, Ord a) => Pretty (Program a) where
  pretty Program {..} =
    vcat
      $ concat
        [ ["; variables"],
          map (\(v, t, e) -> parens $ sep ["define" <+> pretty v, pretty t, pretty e]) topVars,
          ["; functions"],
          map (\(f, ps, t, e) -> parens $ sep [sep ["define" <+> parens (sep $ map pretty $ f : ps), pretty t], pretty e]) topFuns,
          ["; externals"],
          map (\(f, t) -> parens $ sep ["extern", "%" <> pretty f, pretty t]) extFuns
        ]

instance (S.ToSExpr a) => S.ToSExpr (Program a) where
  toSExpr Program {..} =
    S.L
      [ S.L $ map topVar topVars,
        S.L $ map topFun topFuns,
        S.L $ map extFun extFuns
      ]
    where
      topVar (v, t, e) = S.L ["define", S.toSExpr v, S.toSExpr t, S.toSExpr e]
      topFun (f, ps, t, e) = S.L ["define", S.L $ S.toSExpr f : map S.toSExpr ps, S.toSExpr t, S.toSExpr e]
      extFun (f, t) = S.L ["extern", S.A $ S.Symbol f, S.toSExpr t]

instance HasExpr Program where
  expr f Program {..} =
    Program
      <$> traverseOf (traversed . _3) f topVars
      <*> traverseOf (traversed . _4) f topFuns
      <*> pure extFuns

runDef :: Eff (Writer (Endo (Expr (Meta Type))) : es) (Expr (Meta Type)) -> Eff es (Expr (Meta Type))
runDef m = uncurry (flip appEndo) <$> runWriter m

let_ :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo (Expr (Meta Type))) :> es) => Type -> Obj (Meta Type) -> Eff es (Atom (Meta Type))
let_ otype obj = do
  x <- withMeta otype <$> newTemporalId "let"
  tell $ Endo $ \e -> Let [LocalDef x otype obj] e
  pure (Var x)

bind :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo (Expr (Meta Type))) :> es) => Expr (Meta Type) -> Eff es (Atom (Meta Type))
bind (Atom a) = pure a
bind v = do
  x <- withMeta (typeOf v) <$> newTemporalId "d"
  tell $ Endo $ \e ->
    Assign x v e
  pure (Var x)

cast :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo (Expr (Meta Type))) :> es) => Type -> Expr (Meta Type) -> Eff es (Atom (Meta Type))
cast ty e
  | ty == typeOf e = bind e
  | otherwise = do
      v <- bind e
      x <- withMeta ty <$> newTemporalId "cast"
      tell $ Endo $ \e -> Assign x (Cast ty v) e
      pure (Var x)

-- `destruct` is convenient when treating types that have only one constructor.
-- For example, if we can write `let Foo x = v;` as the syntax sugar of `let x = v |> { Foo x -> x | _ -> error }`,
-- we can use `destruct` to support this syntax sugar.
-- But `let Foo x = v` style has some problem:
-- 1. Programmer must check whether the type can be treated that have only one constructor.
-- 2. There is more safe and convenenient way: `if let` in Rust.

-- destruct :: (MonadIO m, MonadReader env m, HasUniqSupply env UniqSupply) => Expr (Meta Type) -> Con -> DefBuilderT m [Atom (Meta Type)]
-- destruct val con@(Con _ ts) = do
--   vs <- traverse (newTemporalId "p") ts
--   DefBuilderT $ tell $ Endo $ \e -> Match val (Unpack con vs e :| [])
--   pure $ map Var vs

callGraph :: (Ord a) => Program a -> (Graph, Vertex -> (a, a, [a]), a -> Maybe Vertex)
callGraph Program {..} =
  let edges = map cgTopVar topVars <> map cgTopFun topFuns
   in graphFromEdges edges
  where
    cgTopVar (a, _, e) = (a, a, Set.toList $ callees e <> freevars e) -- Merge @callees@ and @freevars@ to avoid missing callees used as a closure.
    cgTopFun (a, ps, _, e) = (a, a, Set.toList $ Set.difference (callees e <> freevars e) (Set.fromList ps))

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
      -- | cases
      [(Tag, Expr a)]
      -- | default case
      (Expr a)
  | -- | switch by unboxed value
    SwitchUnboxed
      (Atom a)
      -- | cases
      [(Unboxed, Expr a)]
      -- | default case
      (Expr a)
  | -- | destruct a value
    Destruct (Atom a) Con [a] (Expr a)
  | -- | destruct a record
    DestructRecord (Atom a) (Map Text a) (Expr a)
  | -- | Assign a value to a variable
    Assign
      a
      -- | Value to assign. It does not include @Assign@ directly.
      (Expr a)
      (Expr a)
  | -- | raise an internal error
    Error Type
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

instance (HasType a) => HasType (Expr a) where
  typeOf (Atom x) = typeOf x
  typeOf (Call f xs) = case typeOf f of
    ps :-> r | map typeOf xs == ps -> r
    _ -> errorDoc $ "Invalid type:" <+> squotes (pretty $ typeOf f)
  typeOf (CallDirect f xs) = case typeOf f of
    ps :-> r | map typeOf xs == ps -> r
    _ -> error "typeOf f must be ps :-> r"
  typeOf (RawCall _ t xs) = case t of
    ps :-> r | map typeOf xs == ps -> r
    _ -> error "t must be ps :-> r"
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

instance (Pretty a) => Pretty (Expr a) where
  pretty (Atom x) = pretty x
  pretty (Call f xs) = parens $ "call" <+> pretty f <+> sep (map pretty xs)
  pretty (CallDirect f xs) = parens $ "direct" <+> pretty f <+> sep (map pretty xs)
  pretty (RawCall p t xs) = parens $ "raw" <+> pretty p <+> pretty t <+> sep (map pretty xs)
  pretty (Cast ty x) = parens $ "cast" <+> pretty ty <+> pretty x
  pretty (Let xs e) =
    parens $ vsep ["let", parens (vcat (map pretty xs)), pretty e]
  pretty (Match v cs) = parens $ vsep ["match" <+> pretty v, vcat (toList $ fmap pretty cs)]
  pretty (Switch v cs e) = parens $ vsep ["switch" <+> pretty v, vcat (toList $ fmap prettyCase cs), parens ("default" <+> pretty e)]
    where
      prettyCase (t, e) = parens $ pretty t <+> pretty e
  pretty (SwitchUnboxed v cs e) = parens $ vsep ["switch-unboxed" <+> pretty v, vcat (toList $ fmap prettyCase cs), parens ("default" <+> pretty e)]
    where
      prettyCase (t, e) = parens $ pretty t <+> pretty e
  pretty (Destruct v con xs e) = parens $ vsep ["destruct" <+> pretty v <+> pretty con <+> parens (sep (map pretty xs)), pretty e]
  pretty (DestructRecord v kvs e) =
    parens $ vsep ["destruct-record" <+> pretty v <+> parens (sep (map (\(k, v) -> pretty k <+> pretty v) $ Map.toList kvs)), pretty e]
  pretty (Assign x v e) = parens $ vsep ["=" <+> pretty x <+> pretty v, pretty e]
  pretty (Error t) = parens $ "ERROR" <+> pretty t

instance (S.ToSExpr a) => S.ToSExpr (Expr a) where
  toSExpr (Atom atom) = S.toSExpr atom
  toSExpr (Call f xs) = S.L $ S.A (S.Symbol "call") : S.toSExpr f : map S.toSExpr xs
  toSExpr (CallDirect f xs) = S.L $ S.A (S.Symbol "direct") : S.toSExpr f : map S.toSExpr xs
  toSExpr (RawCall p t xs) = S.L $ S.A (S.Symbol "raw") : S.toSExpr p : S.toSExpr t : map S.toSExpr xs
  toSExpr (Cast ty x) = S.L [S.A $ S.Symbol "cast", S.toSExpr ty, S.toSExpr x]
  toSExpr (Let xs e) = S.L $ S.A (S.Symbol "let") : S.L (map S.toSExpr xs) : [S.toSExpr e]
  toSExpr (Match e cs) = S.L $ S.A (S.Symbol "match") : S.toSExpr e : map S.toSExpr cs
  toSExpr (Switch v cs e) = S.L $ S.A (S.Symbol "switch") : S.toSExpr v : map S.toSExpr cs <> [S.toSExpr e]
  toSExpr (SwitchUnboxed v cs e) = S.L $ S.A (S.Symbol "switch-unboxed") : S.toSExpr v : map S.toSExpr cs <> [S.toSExpr e]
  toSExpr (Destruct v con xs e) = S.L [S.A $ S.Symbol "destruct", S.toSExpr v, S.toSExpr con, S.L $ map S.toSExpr xs, S.toSExpr e]
  toSExpr (DestructRecord v kvs e) = S.L [S.A $ S.Symbol "destruct-record", S.toSExpr v, S.L $ map S.toSExpr $ Map.toList kvs, S.toSExpr e]
  toSExpr (Assign x v e) = S.L [S.A $ S.Symbol "=", S.toSExpr x, S.toSExpr v, S.toSExpr e]
  toSExpr (Error t) = S.L [S.A $ S.Symbol "ERROR", S.toSExpr t]

instance HasFreeVar Expr where
  freevars (Atom x) = freevars x
  freevars (Call f xs) = freevars f <> foldMap freevars xs
  freevars (CallDirect _ xs) = foldMap freevars xs
  freevars (RawCall _ _ xs) = foldMap freevars xs
  freevars (Cast _ x) = freevars x
  freevars (Let xs e) = foldr (sans . (._variable)) (freevars e <> foldMap (freevars . (._object)) xs) xs
  freevars (Match e cs) = freevars e <> foldMap freevars cs
  freevars (Switch v cs e) = freevars v <> foldMap (freevars . snd) cs <> freevars e
  freevars (SwitchUnboxed v cs e) = freevars v <> foldMap (freevars . snd) cs <> freevars e
  freevars (Destruct v _ xs e) =
    freevars v
      <> Set.difference
        (freevars e)
        (Set.fromList xs)
  freevars (DestructRecord v kvs e) =
    freevars v
      <> Set.difference
        (freevars e)
        (Set.fromList $ Map.elems kvs)
  freevars (Assign x v e) = freevars v <> sans x (freevars e)
  freevars (Error _) = mempty
  callees Atom {} = mempty
  callees (Call f _) = freevars f
  callees (CallDirect f _) = Set.singleton f
  callees RawCall {} = mempty
  callees Cast {} = mempty
  callees (Let xs e) = foldr (sans . (._variable)) (callees e <> foldMap (callees . (._object)) xs) xs
  callees (Match e cs) = callees e <> foldMap callees cs
  callees (Switch _ cs e) = foldMap (callees . snd) cs <> callees e
  callees (SwitchUnboxed _ cs e) = foldMap (callees . snd) cs <> callees e
  callees (Destruct _ _ xs e) =
    Set.difference
      (callees e)
      (Set.fromList xs)
  callees (DestructRecord _ kvs e) =
    Set.difference
      (callees e)
      (Set.fromList $ Map.elems kvs)
  callees (Assign x v e) = callees v <> sans x (callees e)
  callees (Error _) = mempty

instance HasAtom Expr where
  atom f = \case
    Atom x -> Atom <$> f x
    Call x xs -> Call <$> f x <*> traverse f xs
    CallDirect x xs -> CallDirect x <$> traverse f xs
    RawCall p t xs -> RawCall p t <$> traverse f xs
    Cast ty x -> Cast ty <$> f x
    Let xs e -> Let <$> traverseOf (traversed . atom) f xs <*> traverseOf atom f e
    Match e cs -> Match <$> traverseOf atom f e <*> traverseOf (traversed . atom) f cs
    Switch v cs e -> Switch <$> f v <*> traverseOf (traversed . _2 . atom) f cs <*> traverseOf atom f e
    SwitchUnboxed v cs e -> SwitchUnboxed <$> f v <*> traverseOf (traversed . _2 . atom) f cs <*> traverseOf atom f e
    Destruct v con xs e -> Destruct <$> f v <*> pure con <*> pure xs <*> traverseOf atom f e
    DestructRecord v kvs e -> DestructRecord <$> f v <*> pure kvs <*> traverseOf atom f e
    Assign x v e -> Assign x <$> traverseOf atom f v <*> traverseOf atom f e
    Error t -> pure (Error t)

instance Plated (Expr a) where
  plate :: Traversal' (Expr a) (Expr a)
  plate _ e@Atom {} = pure e
  plate _ e@Call {} = pure e
  plate _ e@CallDirect {} = pure e
  plate _ e@RawCall {} = pure e
  plate _ e@Cast {} = pure e
  plate f (Let xs e) = Let <$> traverseOf (traversed . expr) f xs <*> f e
  plate f (Match e cs) = Match <$> f e <*> traverseOf (traversed . expr) f cs
  plate f (Switch v cs e) = Switch v <$> traverseOf (traversed . _2) f cs <*> f e
  plate f (SwitchUnboxed v cs e) = SwitchUnboxed v <$> traverseOf (traversed . _2) f cs <*> f e
  plate f (Destruct v con xs e) = Destruct v con xs <$> f e
  plate f (DestructRecord v kvs e) = DestructRecord v kvs <$> f e
  plate f (Assign x v e) = Assign x <$> f v <*> f e
  plate _ e@Error {} = pure e
  {-# INLINE plate #-}

class HasExpr f where
  expr :: Traversal' (f a) (Expr a)

instance HasExpr Expr where
  expr = identity

-- | 'f' may have free variables
-- 'freevars' does not include callees of `call-direct`.
-- If you want to include callees of `call-direct`, merge 'callees' and 'freevars'.
class HasFreeVar f where
  -- | Free variables.
  -- It does not include callees of `call-direct`.
  freevars :: (Ord a) => f a -> Set a

  -- | Callees.
  callees :: (Ord a) => f a -> Set a

-- | atoms
data Atom a
  = -- | variable
    Var a
  | -- | literal of unboxed values
    Unboxed Unboxed
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

instance (HasType a) => HasType (Atom a) where
  typeOf (Var x) = typeOf x
  typeOf (Unboxed x) = typeOf x

instance (Pretty a) => Pretty (Atom a) where
  pretty (Var x) = pretty x
  pretty (Unboxed x) = pretty x

instance (S.ToSExpr a) => S.ToSExpr (Atom a) where
  toSExpr (Var x) = S.toSExpr x
  toSExpr (Unboxed x) = S.toSExpr x

instance HasFreeVar Atom where
  freevars (Var x) = Set.singleton x
  freevars Unboxed {} = mempty
  callees _ = mempty

-- | 'f' may include atoms
class HasAtom f where
  atom :: Traversal' (f a) (Atom a)

instance HasAtom Atom where
  atom = identity

-- | Unboxed values
data Unboxed
  = Int32 Integer
  | Int64 Integer
  | Float Float
  | Double Double
  | Char Char
  | String Text
  | Bool Bool
  deriving stock (Eq, Ord, Show, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

instance HasType Unboxed where
  typeOf Int32 {} = Int32T
  typeOf Int64 {} = Int64T
  typeOf Float {} = FloatT
  typeOf Double {} = DoubleT
  typeOf Char {} = CharT
  typeOf String {} = StringT
  typeOf Bool {} = BoolT

instance Pretty Unboxed where
  pretty (Int32 x) = pretty x <> "_i32"
  pretty (Int64 x) = pretty x <> "_i64"
  pretty (Float x) = pretty (showHex (castFloatToWord32 x) "") <> "_f32" <+> "#|" <> pretty x <> "|#"
  pretty (Double x) = pretty (showHex (castDoubleToWord64 x) "") <> "_f64" <+> "#|" <> pretty x <> "|#"
  pretty (Char x) = squotes (pretty $ convertString @_ @Text $ showLitChar x "")
  pretty (String x) = dquotes (pretty $ concatMap (`showLitChar` "") $ convertString @_ @String x)
  pretty (Bool True) = "True#"
  pretty (Bool False) = "False#"

instance S.ToSExpr Unboxed where
  toSExpr (Int32 x) = S.A $ S.Int x (Just "i32")
  toSExpr (Int64 x) = S.A $ S.Int x (Just "i64")
  toSExpr (Float x) = S.A $ S.Float x
  toSExpr (Double x) = S.A $ S.Double x
  toSExpr (Char x) = S.A $ S.Char x
  toSExpr (String x) = S.A $ S.String x
  toSExpr (Bool True) = S.A $ S.Symbol "True#"
  toSExpr (Bool False) = S.A $ S.Symbol "False#"

-- | Let bindings
data LocalDef a = LocalDef {_variable :: a, typ :: Type, _object :: Obj a}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

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
  pretty (LocalDef v t o) = parens $ vsep [pretty v <+> pretty t, pretty o]

instance (S.ToSExpr a) => S.ToSExpr (LocalDef a) where
  toSExpr (LocalDef v t o) = S.L [S.toSExpr v, S.toSExpr t, S.toSExpr o]

instance HasAtom LocalDef where
  atom = object . atom

-- | heap objects
data Obj a
  = -- | function (arity >= 1)
    Fun [a] (Expr a)
  | -- | saturated constructor (arity >= 0)
    Pack Type Con [Atom a]
  | -- | record
    Record (Map Text (Atom a))
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

instance (HasType a) => HasType (Obj a) where
  typeOf (Fun xs e) = map typeOf xs :-> typeOf e
  typeOf (Pack t _ _) = t
  typeOf (Record kvs) = RecordT (fmap typeOf kvs)

instance (Pretty a) => Pretty (Obj a) where
  pretty (Fun xs e) = parens $ sep ["fun" <+> parens (sep $ map pretty xs), pretty e]
  pretty (Pack ty c xs) = parens $ sep (["pack", pretty ty, pretty c] <> map pretty xs)
  pretty (Record kvs) =
    parens
      $ sep
        [ "record"
            <+> parens
              ( sep
                  $ map
                    ( \(k, v) ->
                        pretty k
                          <+> pretty v
                    )
                    (Map.toList kvs)
              )
        ]

instance (S.ToSExpr a) => S.ToSExpr (Obj a) where
  toSExpr (Fun xs e) = S.L [S.A $ S.Symbol "fun", S.L $ map S.toSExpr xs, S.toSExpr e]
  toSExpr (Pack ty c xs) = S.L $ S.A (S.Symbol "pack") : S.toSExpr ty : S.toSExpr c : map S.toSExpr xs
  toSExpr (Record kvs) = S.L $ S.A (S.Symbol "record") : map S.toSExpr (Map.toList kvs)

instance HasFreeVar Obj where
  freevars (Fun as e) = foldr sans (freevars e) as
  freevars (Pack _ _ xs) = foldMap freevars xs
  freevars (Record kvs) = foldMap freevars kvs
  callees (Fun as e) = foldr sans (callees e) as
  callees Pack {} = mempty
  callees Record {} = mempty

instance HasAtom Obj where
  atom f = \case
    Fun xs e -> Fun xs <$> traverseOf atom f e
    Pack ty con xs -> Pack ty con <$> traverseOf (traversed . atom) f xs
    Record kvs -> Record <$> traverseOf (traversed . atom) f kvs

-- | alternatives
data Case a
  = -- | constructor pattern
    Unpack Con [a] (Expr a)
  | -- | record pattern
    OpenRecord (Map Text a) (Expr a)
  | -- | unboxed value pattern
    Exact Unboxed (Expr a)
  | -- | variable pattern
    Bind a Type (Expr a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

instance (HasType a) => HasType (Case a) where
  typeOf (Unpack _ _ e) = typeOf e
  typeOf (OpenRecord _ e) = typeOf e
  typeOf (Exact _ e) = typeOf e
  typeOf (Bind _ _ e) = typeOf e

instance (Pretty a) => Pretty (Case a) where
  pretty (Unpack c xs e) =
    parens $ sep ["unpack" <+> parens (pretty c <+> sep (map pretty xs)), pretty e]
  pretty (OpenRecord pat e) =
    parens $ sep ["open", parens $ sep $ map (\(k, v) -> pretty k <+> pretty v) $ Map.toList pat, pretty e]
  pretty (Exact u e) = parens $ sep ["exact" <+> pretty u, pretty e]
  pretty (Bind x t e) = parens $ sep ["bind", pretty x, pretty t, pretty e]

instance (S.ToSExpr a) => S.ToSExpr (Case a) where
  toSExpr (Unpack c xs e) = S.L [S.A $ S.Symbol "unpack", S.toSExpr c, S.L $ map S.toSExpr xs, S.toSExpr e]
  toSExpr (OpenRecord pat e) = S.L [S.A $ S.Symbol "open", S.L $ map S.toSExpr $ Map.toList pat, S.toSExpr e]
  toSExpr (Exact u e) = S.L [S.A $ S.Symbol "exact", S.toSExpr u, S.toSExpr e]
  toSExpr (Bind x t e) = S.L [S.A $ S.Symbol "bind", S.toSExpr x, S.toSExpr t, S.toSExpr e]

instance HasFreeVar Case where
  freevars (Unpack _ xs e) = foldr sans (freevars e) xs
  freevars (OpenRecord pat e) = foldr sans (freevars e) (Map.elems pat)
  freevars (Exact _ e) = freevars e
  freevars (Bind x _ e) = sans x $ freevars e
  callees (Unpack _ xs e) = foldr sans (callees e) xs
  callees (OpenRecord pat e) = foldr sans (callees e) (Map.elems pat)
  callees (Exact _ e) = callees e
  callees (Bind x _ e) = sans x $ callees e

instance HasAtom Case where
  atom f = \case
    Unpack con xs e -> Unpack con xs <$> traverseOf atom f e
    OpenRecord pat e -> OpenRecord pat <$> traverseOf atom f e
    Exact u e -> Exact u <$> traverseOf atom f e
    Bind a t e -> Bind a t <$> traverseOf atom f e

instance HasExpr Case where
  expr f = \case
    Unpack con ps e -> Unpack con ps <$> f e
    OpenRecord kvs e -> OpenRecord kvs <$> f e
    Exact u e -> Exact u <$> f e
    Bind x t e -> Bind x t <$> f e

instance HasExpr LocalDef where
  expr = object . expr

instance HasExpr Obj where
  expr f = \case
    Fun ps e -> Fun ps <$> f e
    o -> pure o

makePrisms ''Case
makeStore ''Unboxed
makeStore ''Atom
$( concat
     <$> sequence
       [ makeStore ''Expr,
         makeStore ''Case,
         makeStore ''LocalDef,
         makeStore ''Obj
       ]
 )
makeStore ''Program