{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Malgo.Core.Syntax.Expr
  ( Expr (..),
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

import Control.Lens (Plated (..), Traversal', makePrisms, sans, traverseOf, traversed, _2)
import Data.Aeson (FromJSON (..), KeyValue (..), ToJSON (..), (.:))
import Data.Data (Data)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Store (Store)
import Data.String.Conversions
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Generic.Data
import Malgo.Core.Syntax.Common
import Malgo.Core.Type
import Malgo.Prelude
import Numeric (showHex)
import Test.QuickCheck (Arbitrary (..), oneof)

-- $setup
-- >>> import Data.Aeson (decode, encode)

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
  deriving anyclass (ToJSON, FromJSON, Store)

instance (Arbitrary a) => Arbitrary (Expr a) where
  arbitrary =
    oneof
      [ Atom <$> arbitrary,
        Call <$> arbitrary <*> arbitrary,
        CallDirect <$> arbitrary <*> arbitrary,
        RawCall <$> arbitrary <*> arbitrary <*> arbitrary,
        Cast <$> arbitrary <*> arbitrary,
        Let <$> arbitrary <*> arbitrary,
        Match <$> arbitrary <*> arbitrary,
        Switch <$> arbitrary <*> arbitrary <*> arbitrary,
        SwitchUnboxed <$> arbitrary <*> arbitrary <*> arbitrary,
        Destruct <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        DestructRecord <$> arbitrary <*> arbitrary <*> arbitrary,
        Assign <$> arbitrary <*> arbitrary <*> arbitrary,
        Error <$> arbitrary
      ]

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
    Var a
  | -- | literal of unboxed values
    Unboxed Unboxed
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Store)

instance (Arbitrary a) => Arbitrary (Atom a) where
  arbitrary = oneof [Var <$> arbitrary, Unboxed <$> arbitrary]

-- |
-- prop> \x -> decode (encode x) == Just (x :: Atom String)
instance (ToJSON a) => ToJSON (Atom a) where
  toJSON (Var x) = toJSONTagged "Var" ["variable" .= x]
  toJSON (Unboxed x) = toJSONTagged "Unboxed" ["literal" .= x]

instance (FromJSON a) => FromJSON (Atom a) where
  parseJSON =
    parseJSONTagged
      "Atom"
      [ ("Var", \v -> Var <$> v .: "variable"),
        ("Unboxed", \v -> Unboxed <$> v .: "literal")
      ]

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

instance Arbitrary Unboxed where
  arbitrary =
    oneof
      [ Int32 <$> arbitrary,
        Int64 <$> arbitrary,
        Float <$> arbitrary,
        Double <$> arbitrary,
        Char <$> arbitrary,
        String <$> arbitrary,
        Bool <$> arbitrary
      ]

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
  deriving anyclass (ToJSON, FromJSON, Store)

instance (Arbitrary a) => Arbitrary (LocalDef a) where
  arbitrary = LocalDef <$> arbitrary <*> arbitrary <*> arbitrary

instance (Pretty a) => Pretty (LocalDef a) where
  pretty (LocalDef v t o) = parens $ vsep [pretty v <+> pretty t, pretty o]

instance HasAtom LocalDef where
  atom f LocalDef {..} = LocalDef variable typ <$> atom f object

-- | heap objects
data Obj a
  = -- | function (arity >= 1)
    Fun [a] (Expr a)
  | -- | saturated constructor (arity >= 0)
    Pack Type Con [Atom a]
  | -- | record
    Record (Map Text (Atom a))
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON, Store)

instance (Arbitrary a) => Arbitrary (Obj a) where
  arbitrary =
    oneof
      [ Fun <$> arbitrary <*> arbitrary,
        Pack <$> arbitrary <*> arbitrary <*> arbitrary,
        Record <$> arbitrary
      ]

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
    Unpack Con [a] (Expr a)
  | -- | record pattern
    OpenRecord (Map Text a) (Expr a)
  | -- | unboxed value pattern
    Exact Unboxed (Expr a)
  | -- | variable pattern
    Bind a Type (Expr a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Store)

instance (Arbitrary a) => Arbitrary (Case a) where
  arbitrary =
    oneof
      [ Unpack <$> arbitrary <*> arbitrary <*> arbitrary,
        OpenRecord <$> arbitrary <*> arbitrary,
        Exact <$> arbitrary <*> arbitrary,
        Bind <$> arbitrary <*> arbitrary <*> arbitrary
      ]

-- |
-- prop> \x -> decode (encode x) == Just (x :: Case String)
instance (ToJSON a) => ToJSON (Case a) where
  toJSON (Unpack constructor variables body) = toJSONTagged "Unpack" ["constructor" .= constructor, "variables" .= variables, "body" .= body]
  toJSON (OpenRecord record body) = toJSONTagged "OpenRecord" ["record" .= record, "body" .= body]
  toJSON (Exact literal body) = toJSONTagged "Exact" ["literal" .= literal, "body" .= body]
  toJSON (Bind variable typ body) = toJSONTagged "Bind" ["variable" .= variable, "type" .= typ, "body" .= body]

instance (FromJSON a) => FromJSON (Case a) where
  parseJSON =
    parseJSONTagged
      "Case"
      [ ("Unpack", \v -> Unpack <$> v .: "constructor" <*> v .: "variables" <*> v .: "body"),
        ("OpenRecord", \v -> OpenRecord <$> v .: "record" <*> v .: "body"),
        ("Exact", \v -> Exact <$> v .: "literal" <*> v .: "body"),
        ("Bind", \v -> Bind <$> v .: "variable" <*> v .: "type" <*> v .: "body")
      ]

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
