{-# LANGUAGE DeriveAnyClass #-}

module Koriel.Core.Syntax.Expr (Expr (..), HasExpr (..)) where

import Control.Lens (Plated (..), Traversal', sans, traverseOf, traversed, _2)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.String.Conversions
import Generic.Data
import Koriel.Core.Op
import Koriel.Core.Syntax.Atom
import Koriel.Core.Syntax.Case
import Koriel.Core.Syntax.Common
import Koriel.Core.Syntax.LocalDef
import Koriel.Core.Syntax.Unboxed
import Koriel.Core.Type
import Koriel.Prelude
import Koriel.Pretty

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
    DestructRecord (Atom a) (HashMap Text a) (Expr a)
  | -- | Assign a value to a variable
    Assign
      a
      -- | Value to assign. It does not include @Assign@ directly.
      (Expr a)
      (Expr a)
  | -- | raise an internal error
    Error Type
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Binary, ToJSON, FromJSON)

instance (HasType a) => HasType (Expr a) where
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

instance (Pretty a) => Pretty (Expr a) where
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
  callees Atom {} = mempty
  callees (Call f _) = freevars f
  callees (CallDirect f _) = HashSet.singleton f
  callees RawCall {} = mempty
  callees BinOp {} = mempty
  callees Cast {} = mempty
  callees (Let xs e) = foldr (sans . (._variable)) (callees e <> foldMap (callees . (._object)) xs) xs
  callees (Match e cs) = callees e <> foldMap callees cs
  callees (Switch _ cs e) = foldMap (callees . snd) cs <> callees e
  callees (SwitchUnboxed _ cs e) = foldMap (callees . snd) cs <> callees e
  callees (Destruct _ _ xs e) =
    HashSet.difference
      (callees e)
      (HashSet.fromList xs)
  callees (DestructRecord _ kvs e) =
    HashSet.difference
      (callees e)
      (HashSet.fromList $ HashMap.elems kvs)
  callees (Assign x v e) = callees v <> sans x (callees e)
  callees (Error _) = mempty

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

instance Plated (Expr a) where
  plate :: Traversal' (Expr a) (Expr a)
  plate _ e@Atom {} = pure e
  plate _ e@Call {} = pure e
  plate _ e@CallDirect {} = pure e
  plate _ e@RawCall {} = pure e
  plate _ e@BinOp {} = pure e
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
  expr = object . expr

instance HasExpr Obj where
  expr f = \case
    Fun ps e -> Fun ps <$> f e
    o -> pure o
