{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- malgoの共通中間表現。
-- A正規形に近い。
module Koriel.Core.Syntax where

import Data.Aeson hiding (Bool, Error, String)
import Data.Aeson.Types (prependFailure, unexpected)
import qualified Data.Set as Set
import Koriel.Core.Op
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude hiding ((.=))
import Koriel.Pretty

class HasFreeVar f where
  -- | free variables
  freevars :: Ord a => f a -> Set a

-- | unboxed values
data Unboxed
  = Int32 Integer
  | Int64 Integer
  | Float Float
  | Double Double
  | Char Char
  | String String
  | Bool Bool
  deriving stock (Eq, Ord, Show, Generic)

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
  pPrint (String x) = doubleQuotes (text x)
  pPrint (Bool True) = "True#"
  pPrint (Bool False) = "False#"

instance ToJSON Unboxed where
  toJSON (Int32 i) = object [("type", "Int32"), "value" .= i]
  toJSON (Int64 i) = object [("type", "Int64"), "value" .= i]
  toJSON (Float f) = object [("type", "Float"), "value" .= f]
  toJSON (Double f) = object [("type", "Double"), "value" .= f]
  toJSON (Char c) = object [("type", "Char"), "value" .= c]
  toJSON (String s) = object [("type", "String"), "value" .= s]
  toJSON (Bool b) = object [("type", "Bool"), "value" .= b]

instance FromJSON Unboxed where
  parseJSON = withObject "Unboxed" $ \v -> do
    ty <- v .: "type"
    if
        | ty == ("Int32" :: String) -> Int32 <$> v .: "value"
        | ty == "Int64" -> Int64 <$> v .: "value"
        | ty == "Float" -> Float <$> v .: "value"
        | ty == "Double" -> Double <$> v .: "value"
        | ty == "Char" -> Char <$> v .: "value"
        | ty == "String" -> String <$> v .: "value"
        | ty == "Bool" -> Bool <$> v .: "value"
        | otherwise -> prependFailure "parsing Unboxed failed, " (unexpected $ Object v)

-- | atoms
data Atom a
  = -- | variable
    Var a
  | -- | literal of unboxed values
    Unboxed Unboxed
  deriving stock (Eq, Show, Functor, Foldable, Generic)

instance HasType a => HasType (Atom a) where
  typeOf (Var x) = typeOf x
  typeOf (Unboxed x) = typeOf x

instance Pretty a => Pretty (Atom a) where
  pPrint (Var x) = pPrint x
  pPrint (Unboxed x) = pPrint x

instance ToJSON a => ToJSON (Atom a) where
  toJSON (Var x) = object [("type", "Var"), "variable" .= x]
  toJSON (Unboxed x) = object [("type", "Unboxed"), "unboxed" .= x]

instance FromJSON a => FromJSON (Atom a) where
  parseJSON = withObject "Atom" $ \v -> do
    ty <- v .: "type"
    if
        | ty == ("Var" :: String) -> Var <$> v .: "variable"
        | ty == "Unboxed" -> Unboxed <$> v .: "unboxed"
        | otherwise -> prependFailure "parsing Atom failed, " (unexpected $ Object v)

instance HasFreeVar Atom where
  freevars (Var x) = Set.singleton x
  freevars Unboxed {} = mempty

class HasAtom f where
  atom :: Traversal' (f a) (Atom a)

instance HasAtom Atom where
  atom = id

data LocalDef a = LocalDef {_localDefVar :: a, _localDefObj :: Obj a}
  deriving stock (Eq, Show, Functor, Foldable, Generic)

localDefVar :: Lens' (LocalDef a) a
localDefVar = lens _localDefVar (\l v -> l {_localDefVar = v})

localDefObj :: Lens' (LocalDef a) (Obj a)
localDefObj = lens _localDefObj (\l o -> l {_localDefObj = o})

instance ToJSON a => ToJSON (LocalDef a) where
  toJSON (LocalDef x o) = object [("type", "LocalDef"), "variable" .= x, "object" .= o]

instance FromJSON a => FromJSON (LocalDef a) where
  parseJSON = withObject "LocalDef" $ \v -> do
    ty <- v .: "type"
    if ty == ("LocalDef" :: String)
      then LocalDef <$> v .: "variable" <*> v .: "object"
      else prependFailure "parsing LocalDef failed, " (unexpected $ Object v)

-- | expressions
data Exp a
  = -- | atom (variables and literals)
    Atom (Atom a)
  | -- | application of closure
    Call (Atom a) [Atom a]
  | -- | application of function (not closure)
    CallDirect a [Atom a]
  | -- | application of external function
    ExtCall String Type [Atom a]
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
  deriving stock (Eq, Show, Functor, Foldable, Generic)

instance HasType a => HasType (Exp a) where
  typeOf (Atom x) = typeOf x
  typeOf (Call f xs) = case typeOf f of
    ps :-> r -> go ps (map typeOf xs) r
    _ -> errorDoc $ "Invalid type:" <+> quotes (pPrint $ typeOf f)
    where
      go [] [] v = v
      go (p : ps) (x : xs) v = replaceOf tyVar p x (go ps xs v)
      go _ _ _ = bug Unreachable
  typeOf (CallDirect f xs) = case typeOf f of
    ps :-> r -> go ps (map typeOf xs) r
    _ -> bug Unreachable
    where
      go [] [] v = v
      go (p : ps) (x : xs) v = replaceOf tyVar p x (go ps xs v)
      go _ _ _ = bug Unreachable
  typeOf (ExtCall _ t xs) = case t of
    ps :-> r -> go ps (map typeOf xs) r
    _ -> bug Unreachable
    where
      go [] [] v = v
      go (p : ps) (x : xs) v = replaceOf tyVar p x (go ps xs v)
      go _ _ _ = bug Unreachable
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
  pPrint (ExtCall p t xs) = parens $ "external" <+> text p <> braces (pPrint t) <+> sep (map pPrint xs)
  pPrint (BinOp o x y) = parens $ pPrint o <+> pPrint x <+> pPrint y
  pPrint (Cast ty x) = parens $ "cast" <+> pPrint ty <+> pPrint x
  pPrint (Let xs e) =
    parens $ "let" $$ parens (vcat (map (\(LocalDef v o) -> parens $ pPrint v $$ pPrint o) xs)) $$ pPrint e
  pPrint (Match v cs) = parens $ "match" <+> pPrint v $$ vcat (toList $ fmap pPrint cs)
  pPrint (Error _) = "ERROR"

instance ToJSON a => ToJSON (Exp a) where
  toJSON (Atom x) = object [("type", "Atom"), "atom" .= x]
  toJSON (Call f xs) = object [("type", "Call"), "func" .= f, "args" .= xs]
  toJSON (CallDirect f xs) = object [("type", "CallDirect"), "func" .= f, "args" .= xs]
  toJSON (ExtCall p t xs) = object [("type", "ExtCall"), "ext_func" .= p, "func_type" .= t, "args" .= xs]
  toJSON (BinOp o x y) = object [("type", "BinOp"), "op" .= o, "left" .= x, "right" .= y]
  toJSON (Cast ty x) = object [("type", "Cast"), "as_type" .= ty, "value" .= x]
  toJSON (Let xs e) = object [("type", "Let"), "definitions" .= xs, "expr" .= e]
  toJSON (Match v cs) = object [("type", "Match"), "scrutinee" .= v, "clauses" .= cs]
  toJSON (Error t) = object [("type", "Error"), "error_type" .= t]

instance FromJSON a => FromJSON (Exp a) where
  parseJSON = withObject "Exp" $ \v -> do
    ty <- v .: "type"
    if
        | ty == ("Atom" :: String) -> Atom <$> v .: "atom"
        | ty == "Call" -> Call <$> v .: "func" <*> v .: "args"
        | ty == "CallDirect" -> CallDirect <$> v .: "func" <*> v .: "args"
        | ty == "ExtCall" -> ExtCall <$> v .: "ext_func" <*> v .: "func_type" <*> v .: "args"
        | ty == "BinOp" -> BinOp <$> v .: "op" <*> v .: "left" <*> v .: "right"
        | ty == "Cast" -> Cast <$> v .: "as_type" <*> v .: "value"
        | ty == "Let" -> Let <$> v .: "definitions" <*> v .: "expr"
        | ty == "Match" -> Match <$> v .: "scrutinee" <*> v .: "clauses"
        | ty == "Error" -> Error <$> v .: "error_type"
        | otherwise -> prependFailure "parsing Exp failed, " (unexpected $ Object v)

instance HasFreeVar Exp where
  freevars (Atom x) = freevars x
  freevars (Call f xs) = freevars f <> foldMap freevars xs
  freevars (CallDirect _ xs) = foldMap freevars xs
  freevars (ExtCall _ _ xs) = foldMap freevars xs
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
    ExtCall p t xs -> ExtCall p t <$> traverse f xs
    BinOp o x y -> BinOp o <$> f x <*> f y
    Cast ty x -> Cast ty <$> f x
    Let xs e -> Let <$> traverseOf (traversed . localDefObj . atom) f xs <*> traverseOf atom f e
    Match e cs -> Match <$> traverseOf atom f e <*> traverseOf (traversed . atom) f cs
    Error t -> pure (Error t)

-- | alternatives
data Case a
  = -- | constructor pattern
    Unpack Con [a] (Exp a)
  | -- | unboxed value pattern
    Switch Unboxed (Exp a)
  | -- | variable pattern
    Bind a (Exp a)
  deriving stock (Eq, Show, Functor, Foldable, Generic)

instance HasType a => HasType (Case a) where
  typeOf (Unpack _ _ e) = typeOf e
  typeOf (Switch _ e) = typeOf e
  typeOf (Bind _ e) = typeOf e

instance Pretty a => Pretty (Case a) where
  pPrint (Unpack c xs e) =
    parens $ sep ["unpack" <+> parens (pPrint c <+> sep (map pPrint xs)), pPrint e]
  pPrint (Switch u e) = parens $ sep ["switch" <+> pPrint u, pPrint e]
  pPrint (Bind x e) = parens $ sep ["bind" <+> pPrint x, pPrint e]

instance ToJSON a => ToJSON (Case a) where
  toJSON (Unpack con vs e) = object [("type", "Unpack"), "con" .= con, "bindings" .= vs, "expr" .= e]
  toJSON (Switch unboxed e) = object [("type", "Switch"), "value" .= unboxed, "expr" .= e]
  toJSON (Bind x e) = object [("type", "Bind"), "binding" .= x, "expr" .= e]

instance FromJSON a => FromJSON (Case a) where
  parseJSON = withObject "Case" $ \v -> do
    ty <- v .: "type"
    if
        | ty == ("Unpack" :: String) -> Unpack <$> v .: "con" <*> v .: "bindings" <*> v .: "expr"
        | ty == "Switch" -> Switch <$> v .: "value" <*> v .: "expr"
        | ty == "Bind" -> Bind <$> v .: "binding" <*> v .: "expr"
        | otherwise -> prependFailure "parsing Case failed, " (unexpected $ Object v)

instance HasFreeVar Case where
  freevars (Unpack _ xs e) = foldr sans (freevars e) xs
  freevars (Switch _ e) = freevars e
  freevars (Bind x e) = sans x $ freevars e

instance HasAtom Case where
  atom f = \case
    Unpack con xs e -> Unpack con xs <$> traverseOf atom f e
    Switch u e -> Switch u <$> traverseOf atom f e
    Bind a e -> Bind a <$> traverseOf atom f e

-- | heap objects
data Obj a
  = -- | function (arity >= 1)
    Fun [a] (Exp a)
  | -- | saturated constructor (arity >= 0)
    Pack Type Con [Atom a]
  deriving stock (Eq, Show, Functor, Foldable, Generic)

instance HasType a => HasType (Obj a) where
  typeOf (Fun xs e) = map typeOf xs :-> typeOf e
  typeOf (Pack t _ _) = t

instance Pretty a => Pretty (Obj a) where
  pPrint (Fun xs e) = parens $ sep ["fun" <+> parens (sep $ map pPrint xs), pPrint e]
  pPrint (Pack ty c xs) = parens $ sep (["pack", pPrint c] <> map pPrint xs) <+> ":" <+> pPrint ty

instance ToJSON a => ToJSON (Obj a) where
  toJSON (Fun ps e) = object [("type", "Fun"), "params" .= ps, "expr" .= e]
  toJSON (Pack ty con as) = object [("type", "Pack"), "pack_to" .= ty, "con" .= con, "args" .= as]

instance FromJSON a => FromJSON (Obj a) where
  parseJSON = withObject "Obj" $ \v -> do
    ty <- v .: "type"
    if
        | ty == ("Fun" :: String) -> Fun <$> v .: "params" <*> v .: "expr"
        | ty == "Pack" -> Pack <$> v .: "pack_to" <*> v .: "con" <*> v .: "args"
        | otherwise -> prependFailure "parsing Obj failed, " (unexpected $ Object v)

instance HasFreeVar Obj where
  freevars (Fun as e) = foldr sans (freevars e) as
  freevars (Pack _ _ xs) = foldMap freevars xs

instance HasAtom Obj where
  atom f = \case
    Fun xs e -> Fun xs <$> traverseOf atom f e
    Pack ty con xs -> Pack ty con <$> traverseOf (traversed . atom) f xs

-- | toplevel function definitions
newtype Program a = Program
  { topFuncs :: [(a, ([a], Exp a))]
  }
  deriving stock (Eq, Show, Functor, Generic)

instance Pretty a => Pretty (Program a) where
  pPrint Program {topFuncs} =
    vcat $
      map
        (\(f, (ps, e)) -> parens $ "define" <+> pPrint f <+> parens (sep $ map pPrint ps) $$ pPrint e)
        topFuncs

instance ToJSON a => ToJSON (Program a) where
  toJSON (Program ds) = toJSON $ map toJSON' ds
    where
      toJSON' (x, (ps, e)) = object ["name" .= x, "params" .= ps, "expr" .= e]

instance FromJSON a => FromJSON (Program a) where
  parseJSON = withArray "Program" $ \v -> do
    let v' = toList v
    Program <$> traverse parseJSON' v'
    where
      parseJSON' = withObject "topFuncs" $ \v -> do
        (,) <$> v .: "name" <*> ((,) <$> v .: "params" <*> v .: "expr")

appObj :: Traversal' (Obj a) (Exp a)
appObj f = \case
  Fun ps e -> Fun ps <$> f e
  o -> pure o

appCase :: Traversal' (Case a) (Exp a)
appCase f = \case
  Unpack con ps e -> Unpack con ps <$> f e
  Switch u e -> Switch u <$> f e
  Bind x e -> Bind x <$> f e

appProgram :: Traversal' (Program a) (Exp a)
appProgram f Program {topFuncs} =
  Program <$> traverseOf (traversed . _2 . _2) f topFuncs

newtype DefBuilderT m a = DefBuilderT {unDefBuilderT :: WriterT (Endo (Exp (Id Type))) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadUniq, MonadIO, MonadTrans, MonadState s, MonadReader r)

runDef :: Functor f => DefBuilderT f (Exp (Id Type)) -> f (Exp (Id Type))
runDef m = uncurry (flip appEndo) <$> runWriterT (unDefBuilderT m)

let_ :: MonadUniq m => Type -> Obj (Id Type) -> DefBuilderT m (Atom (Id Type))
let_ otype obj = do
  x <- newLocalId "$let" otype
  DefBuilderT $ tell $ Endo $ \e -> Let [LocalDef x obj] e
  pure (Var x)

destruct :: MonadUniq m => Exp (Id Type) -> Con -> DefBuilderT m [Atom (Id Type)]
destruct val con@(Con _ ts) = do
  vs <- traverse (newLocalId "$p") ts
  DefBuilderT $ tell $ Endo $ \e -> Match val (Unpack con vs e :| [])
  pure $ map Var vs

bind :: MonadUniq m => Exp (Id Type) -> DefBuilderT m (Atom (Id Type))
bind (Atom a) = pure a
bind v = do
  x <- newLocalId "$d" (typeOf v)
  DefBuilderT $ tell $ Endo $ \e -> Match v (Bind x e :| [])
  pure (Var x)

cast :: MonadUniq m => Type -> Exp (Id Type) -> DefBuilderT m (Atom (Id Type))
cast ty e
  | ty == typeOf e = bind e
  | otherwise = do
    v <- bind e
    x <- newLocalId "$cast" ty
    DefBuilderT $ tell $ Endo $ \e -> Match (Cast ty v) (Bind x e :| [])
    pure (Var x)

mainFunc :: (MonadUniq m) => Exp (Id Type) -> m (Id Type, ([Id Type], Exp (Id Type)))
mainFunc e = do
  mainFuncId <- newGlobalId "main" ([] :-> Int32T)
  mainFuncBody <- runDef $ do
    _ <- bind $ ExtCall "GC_init" ([] :-> VoidT) []
    pure e
  pure (mainFuncId, ([], mainFuncBody))
