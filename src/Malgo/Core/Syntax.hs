{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

-- | AST definitions for Core language
module Malgo.Core.Syntax
  ( Program (..),
    runDef,
    let_,
    bind,
    cast,
    callGraph,
    HasFreeVar (..),
    Expr (..),
    HasExpr (..),
    Atom (..),
    HasAtom (..),
    Unboxed (..),
    LocalDef (..),
    Obj (..),
    Case (..),
    _Unpack,
    _OpenRecord,
    _Exact,
    _Bind,
  )
where

import Control.Lens (Plated (..), Traversal', makePrisms, sans, traverseOf, traversed, _2, _3, _4)
import Data.Aeson (FromJSON (..), KeyValue (..), ToJSON (..), Value, camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, object, withObject, (.:))
import Data.Aeson.Types (Pair)
import Data.Data (Data)
import Data.Graph
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Store (Store)
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
import Numeric (showHex)

-- | toplevel function definitions
data Program a = Program
  { variables :: [(a, Type, Expr a)],
    functions :: [(a, [a], Type, Expr a)],
    externals :: [(Text, Type)]
  }
  deriving stock (Eq, Show, Functor, Generic)
  deriving (Semigroup, Monoid) via Generically (Program a)
  deriving anyclass (Store)

instance (ToJSON a) => ToJSON (Program a) where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance (FromJSON a) => FromJSON (Program a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance (Pretty a, Ord a) => Pretty (Program a) where
  pretty Program {..} =
    vcat
      $ concat
        [ ["; variables"],
          map
            ( \(v, t, e) ->
                -- parens $ sep ["define" <+> pretty v, pretty t, pretty e]
                sexpr ["define", pretty v, pretty t, pretty e]
            )
            variables,
          ["; functions"],
          map
            ( \(f, ps, t, e) ->
                -- parens $ sep [sep ["define" <+> parens (sep $ map pretty $ f : ps), pretty t], pretty e]
                sexpr ["define", sexpr $ map pretty $ f : ps, pretty t, pretty e]
            )
            functions,
          ["; externals"],
          map
            ( \(f, t) ->
                -- parens $ sep ["extern", "%" <> pretty f, pretty t]
                sexpr ["extern", "%" <> pretty f, pretty t]
            )
            externals
        ]

instance HasExpr Program where
  expr f Program {..} =
    Program
      <$> traverseOf (traversed . _3) f variables
      <*> traverseOf (traversed . _4) f functions
      <*> pure externals

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
  let edges = map cgTopVar variables <> map cgTopFun functions
   in graphFromEdges edges
  where
    cgTopVar (a, _, e) = (a, a, Set.toList $ callees e <> freevars e) -- Merge @callees@ and @freevars@ to avoid missing callees used as a closure.
    cgTopFun (a, ps, _, e) = (a, a, Set.toList $ Set.difference (callees e <> freevars e) (Set.fromList ps))

-- | 'f' may have free variables
-- 'freevars' does not include callees of `call-direct`.
-- If you want to include callees of `call-direct`, merge 'callees' and 'freevars'.
class HasFreeVar f where
  -- | Free variables.
  -- It does not include callees of `call-direct`.
  freevars :: (Ord a) => f a -> Set a

  -- | Callees.
  callees :: (Ord a) => f a -> Set a

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
  deriving anyclass (Store)

instance (ToJSON a) => ToJSON (Expr a) where
  toJSON (Atom x) = withTag "Atom" ["atom" .= x]
  toJSON (Call f xs) = withTag "Call" ["callee" .= f, "arguments" .= xs]
  toJSON (CallDirect f xs) = withTag "CallDirect" ["callee" .= f, "arguments" .= xs]
  toJSON (RawCall p t xs) = withTag "RawCall" ["name" .= p, "type" .= t, "arguments" .= xs]
  toJSON (Cast ty x) = withTag "Cast" ["type" .= ty, "value" .= x]
  toJSON (Let xs e) = withTag "Let" ["bindings" .= xs, "body" .= e]
  toJSON (Match e cs) = withTag "Match" ["scrutinee" .= e, "clauses" .= cs]
  toJSON (Switch v cs e) = withTag "Switch" ["scrutinee" .= v, "clauses" .= cs, "default" .= e]
  toJSON (SwitchUnboxed v cs e) = withTag "SwitchUnboxed" ["scrutinee" .= v, "clauses" .= cs, "default" .= e]
  toJSON (Destruct v con xs e) = withTag "Destruct" ["scrutinee" .= v, "constructor" .= con, "variables" .= xs, "body" .= e]
  toJSON (DestructRecord v kvs e) = withTag "DestructRecord" ["scrutinee" .= v, "fields" .= kvs, "body" .= e]
  toJSON (Assign x v e) = withTag "Assign" ["variable" .= x, "expression" .= v, "body" .= e]
  toJSON (Error t) = withTag "Error" ["type" .= t]

withTag :: Text -> [Pair] -> Value
withTag tag pairs = object $ "tag" .= tag : pairs

instance (FromJSON a) => FromJSON (Expr a) where
  parseJSON = withObject "Expr" $ \v -> do
    tag <- v .: "tag"
    case tag :: Text of
      "Atom" -> Atom <$> v .: "atom"
      "Call" -> Call <$> v .: "callee" <*> v .: "arguments"
      "CallDirect" -> CallDirect <$> v .: "callee" <*> v .: "arguments"
      "RawCall" -> RawCall <$> v .: "name" <*> v .: "type" <*> v .: "arguments"
      "Cast" -> Cast <$> v .: "type" <*> v .: "value"
      "Let" -> Let <$> v .: "bindings" <*> v .: "body"
      "Match" -> Match <$> v .: "scrutinee" <*> v .: "clauses"
      "Switch" -> Switch <$> v .: "scrutinee" <*> v .: "clauses" <*> v .: "default"
      "SwitchUnboxed" -> SwitchUnboxed <$> v .: "scrutinee" <*> v .: "clauses" <*> v .: "default"
      "Destruct" -> Destruct <$> v .: "scrutinee" <*> v .: "constructor" <*> v .: "variables" <*> v .: "body"
      "DestructRecord" -> DestructRecord <$> v .: "scrutinee" <*> v .: "fields" <*> v .: "body"
      "Assign" -> Assign <$> v .: "variable" <*> v .: "expresison" <*> v .: "body"
      "Error" -> Error <$> v .: "type"
      _ -> fail $ "Unknown tag: " <> convertString tag

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

instance HasFreeVar Expr where
  freevars (Atom x) = freevars x
  freevars (Call f xs) = freevars f <> foldMap freevars xs
  freevars (CallDirect _ xs) = foldMap freevars xs
  freevars (RawCall _ _ xs) = foldMap freevars xs
  freevars (Cast _ x) = freevars x
  freevars (Let xs e) = foldr (sans . (.variable)) (freevars e <> foldMap (freevars . (.object)) xs) xs
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
  callees (Let xs e) = foldr (sans . (.variable)) (callees e <> foldMap (callees . (.object)) xs) xs
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

instance HasExpr Case where
  expr f = \case
    Unpack con ps e -> Unpack con ps <$> f e
    OpenRecord kvs e -> OpenRecord kvs <$> f e
    Exact u e -> Exact u <$> f e
    Bind x t e -> Bind x t <$> f e

instance HasExpr LocalDef where
  expr f LocalDef {..} = LocalDef variable typ <$> expr f object

instance HasExpr Obj where
  expr f = \case
    Fun ps e -> Fun ps <$> f e
    o -> pure o

-- | atoms
data Atom a
  = -- | variable
    Var {variable :: a}
  | -- | literal of unboxed values
    Unboxed {literal :: Unboxed}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Store, ToJSON, FromJSON)

instance (HasType a) => HasType (Atom a) where
  typeOf (Var x) = typeOf x
  typeOf (Unboxed x) = typeOf x

instance (Pretty a) => Pretty (Atom a) where
  pretty (Var x) = pretty x
  pretty (Unboxed x) = pretty x

instance HasFreeVar Atom where
  freevars (Var x) = Set.singleton x
  freevars Unboxed {} = mempty
  callees _ = mempty

-- | 'f' may include atoms
class HasAtom f where
  atom :: Traversal' (f a) (Atom a)

instance HasAtom Atom where
  atom = identity

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
  deriving anyclass (ToJSON, FromJSON, Store)

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

-- | Let bindings
data LocalDef a = LocalDef {variable :: a, typ :: Type, object :: Obj a}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Store)

instance (ToJSON a) => ToJSON (LocalDef a) where
  toJSON LocalDef {variable, typ, object = obj} = object ["variable" .= variable, "type" .= typ, "object" .= obj]

instance (FromJSON a) => FromJSON (LocalDef a) where
  parseJSON = withObject "LocalDef" $ \v -> LocalDef <$> v .: "variable" <*> v .: "type" <*> v .: "object"

instance (Pretty a) => Pretty (LocalDef a) where
  pretty (LocalDef v t o) = parens $ vsep [pretty v <+> pretty t, pretty o]

instance HasAtom LocalDef where
  atom f LocalDef {..} = LocalDef variable typ <$> atom f object

-- | heap objects
data Obj a
  = -- | function (arity >= 1)
    Fun {parameters :: [a], body :: Expr a}
  | -- | saturated constructor (arity >= 0)
    Pack {typ :: Type, constructor :: Con, arguments :: [Atom a]}
  | -- | record
    Record {fields :: Map Text (Atom a)}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Store)

instance (ToJSON a) => ToJSON (Obj a) where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier =
            \field -> if field == "typ" then "type" else field
        }

instance (FromJSON a) => FromJSON (Obj a) where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier =
            \field -> if field == "typ" then "type" else field
        }

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
    Unpack {constructor :: Con, variables :: [a], body :: Expr a}
  | -- | record pattern
    OpenRecord {fields :: Map Text a, body :: Expr a}
  | -- | unboxed value pattern
    Exact {literal :: Unboxed, body :: Expr a}
  | -- | variable pattern
    Bind {variable :: a, typ :: Type, body :: Expr a}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Store)

instance (ToJSON a) => ToJSON (Case a) where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier =
            \field -> if field == "typ" then "type" else field
        }

instance (FromJSON a) => FromJSON (Case a) where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier =
            \field -> if field == "typ" then "type" else field
        }

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

makePrisms ''Case
