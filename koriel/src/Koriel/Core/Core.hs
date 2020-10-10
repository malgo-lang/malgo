{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-|
malgoとgriffの共通中間表現。
A正規形に近い。
-}
module Koriel.Core.Core where

import qualified Data.Set as Set
import Koriel.Core.Op
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty
import qualified Text.PrettyPrint.HughesPJ as P

class HasFreeVar f where
  freevars :: Ord a => f a -> Set a

{-
Unboxed values  unboxed
-}
data Unboxed
  = Int32 Integer
  | Int64 Integer
  | Float Float
  | Double Double
  | Char Char
  | String String
  deriving stock (Eq, Ord, Show)

instance HasType Unboxed where
  typeOf Int32 {} = Int32T
  typeOf Int64 {} = Int64T
  typeOf Float {} = FloatT
  typeOf Double {} = DoubleT
  typeOf Char {} = CharT
  typeOf String {} = StringT

instance Pretty Unboxed where
  pPrint (Int32 x) = pPrint x
  pPrint (Int64 x) = pPrint x
  pPrint (Float x) = pPrint x
  pPrint (Double x) = pPrint x
  pPrint (Char x) = quotes (pPrint x)
  pPrint (String x) = doubleQuotes (text x)

{-
Atoms  a ::= unboxed | x
-}
data Atom a
  = Var a
  | Unboxed Unboxed
  deriving stock (Eq, Show, Functor, Foldable)

instance HasType a => HasType (Atom a) where
  typeOf (Var x) = typeOf x
  typeOf (Unboxed x) = typeOf x

instance Pretty a => Pretty (Atom a) where
  pPrint (Var x) = pPrint x
  pPrint (Unboxed x) = pPrint x

instance HasFreeVar Atom where
  freevars (Var x) = Set.singleton x
  freevars Unboxed {} = mempty

class HasAtom f where
  atom :: Traversal' (f a) (Atom a)

instance HasAtom Atom where
  atom = castOptic equality

{-
Expressions  e ::= a               Atom
                 | f a_1 ... a_n   Function call (arity(f) >= 1)
                 | p a_1 ... a_n   Saturated primitive operation (n >= 1)
                 | a_1[a_2]        Read array
                 | a_1[a_2] <- a_3 Write array
                 | LET x = obj IN e
                 | MATCH e WITH { alt_1; ... alt_n; } (n >= 0)
-}
data Exp a
  = Atom (Atom a)
  | Call (Atom a) [Atom a]
  | CallDirect a [Atom a]
  | PrimCall String Type [Atom a]
  | BinOp Op (Atom a) (Atom a)
  | ArrayRead (Atom a) (Atom a)
  | ArrayWrite (Atom a) (Atom a) (Atom a)
  | Cast Type (Atom a)
  | Let [(a, Obj a)] (Exp a)
  | Match (Exp a) (NonEmpty (Case a))
  | Error Type
  deriving stock (Eq, Show, Functor, Foldable)

instance HasType a => HasType (Exp a) where
  typeOf (Atom x) = typeOf x
  typeOf (Call f xs) = case typeOf f of
    ps :-> r -> go ps (map typeOf xs) r
    _ -> errorDoc $ "Invalid type:" <+> P.quotes (pPrint $ typeOf f)
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
  typeOf (PrimCall _ t xs) = case t of
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
      boolT = SumT [Con "True" [], Con "False" []]
  typeOf (ArrayRead a _) = case typeOf a of
    ArrayT t -> t
    _ -> bug Unreachable
  typeOf ArrayWrite {} = SumT [Con "Tuple0" []]
  typeOf (Cast ty _) = ty
  typeOf (Let _ e) = typeOf e
  typeOf (Match _ (c :| _)) = typeOf c
  typeOf (Error t) = t

instance Pretty a => Pretty (Exp a) where
  pPrint (Atom x) = pPrint x
  pPrint (Call f xs) = parens $ pPrint f <+> sep (map pPrint xs)
  pPrint (CallDirect f xs) = parens $ "direct" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (PrimCall p _ xs) = parens $ "prim" <+> text p <+> sep (map pPrint xs)
  pPrint (BinOp o x y) = parens $ pPrint o <+> pPrint x <+> pPrint y
  pPrint (ArrayRead a b) = pPrint a <> brackets (pPrint b)
  pPrint (ArrayWrite a b c) = parens $ pPrint a <> brackets (pPrint b) <+> "<-" <+> pPrint c
  pPrint (Cast ty x) = parens $ "cast" <+> pPrint ty <+> pPrint x
  pPrint (Let xs e) =
    parens $ "let" $$ parens (vcat (map (\(v, o) -> parens $ pPrint v $$ pPrint o) xs)) $$ pPrint e
  pPrint (Match v cs) = parens $ "match" <+> pPrint v $$ vcat (toList $ fmap pPrint cs)
  pPrint (Error _) = "ERROR"

instance HasFreeVar Exp where
  freevars (Atom x) = freevars x
  freevars (Call f xs) = freevars f <> foldMap freevars xs
  freevars (CallDirect _ xs) = foldMap freevars xs
  freevars (PrimCall _ _ xs) = foldMap freevars xs
  freevars (BinOp _ x y) = freevars x <> freevars y
  freevars (ArrayRead a b) = freevars a <> freevars b
  freevars (ArrayWrite a b c) = freevars a <> freevars b <> freevars c
  freevars (Cast _ x) = freevars x
  freevars (Let xs e) = foldr (sans . view _1) (freevars e <> foldMap (freevars . view _2) xs) xs
  freevars (Match e cs) = freevars e <> foldMap freevars cs
  freevars (Error _) = mempty

instance HasAtom Exp where
  atom = traversalVL $ \f -> \case
    Atom x -> Atom <$> f x
    Call x xs -> Call <$> f x <*> traverse f xs
    CallDirect x xs -> CallDirect x <$> traverse f xs
    PrimCall p t xs -> PrimCall p t <$> traverse f xs
    BinOp o x y -> BinOp o <$> f x <*> f y
    ArrayRead a b -> ArrayRead <$> f a <*> f b
    ArrayWrite a b c -> ArrayWrite <$> f a <*> f b <*> f c
    Cast ty x -> Cast ty <$> f x
    Let xs e -> Let <$> traverseOf (traversed % _2 % atom) f xs <*> traverseOf atom f e
    Match e cs -> Match <$> traverseOf atom f e <*> traverseOf (traversed % atom) f cs
    Error t -> pure (Error t)

{-
Alternatives  alt ::= UNPACK(C x_1 ... x_n) -> e  (n >= 0)
                    | SWITCH u -> e
                    | BIND x -> e
-}
data Case a
  = Unpack Con [a] (Exp a)
  | Switch Unboxed (Exp a)
  | Bind a (Exp a)
  deriving stock (Eq, Show, Functor, Foldable)

instance Pretty a => Pretty (Case a) where
  pPrint (Unpack c xs e) =
    parens $ sep ["unpack" <+> parens (pPrint c <+> sep (map pPrint xs)), pPrint e]
  pPrint (Switch u e) = parens $ sep ["switch" <+> pPrint u, pPrint e]
  pPrint (Bind x e) = parens $ sep ["bind" <+> pPrint x, pPrint e]

instance HasFreeVar Case where
  freevars (Unpack _ xs e) = foldr sans (freevars e) xs
  freevars (Switch _ e) = freevars e
  freevars (Bind x e) = sans x $ freevars e

instance HasType a => HasType (Case a) where
  typeOf (Unpack _ _ e) = typeOf e
  typeOf (Switch _ e) = typeOf e
  typeOf (Bind _ e) = typeOf e

instance HasAtom Case where
  atom = traversalVL $ \f -> \case
    Unpack con xs e -> Unpack con xs <$> traverseOf atom f e
    Switch u e -> Switch u <$> traverseOf atom f e
    Bind a e -> Bind a <$> traverseOf atom f e

{-
Heap objects  obj ::= FUN(x_1 ... x_n -> e)  Function (arity = n >= 1)
                    | PAP(f a_1 ... a_n)     Partial application (f is always a FUN with arity(f) > n >= 1)
                    | PACK(C a_1 ... a_n)    Saturated constructor (n >= 0)
                    | ARRAY(a, n)            Array (n > 0)
-}
data Obj a
  = Fun [a] (Exp a)
  | Pack Type Con [Atom a]
  | Array (Atom a) (Atom a)
  deriving stock (Eq, Show, Functor, Foldable)

instance Pretty a => Pretty (Obj a) where
  pPrint (Fun xs e) = parens $ sep ["fun" <+> parens (sep $ map pPrint xs), pPrint e]
  pPrint (Pack ty c xs) = parens $ sep (["pack", pPrint c] <> map pPrint xs) <+> ":" <+> pPrint ty
  pPrint (Array a n) = parens $ sep ["array", pPrint a, pPrint n]

instance HasFreeVar Obj where
  freevars (Fun as e) = foldr sans (freevars e) as
  freevars (Pack _ _ xs) = foldMap freevars xs
  freevars (Array a n) = freevars a <> freevars n

instance HasType a => HasType (Obj a) where
  typeOf (Fun xs e) = map typeOf xs :-> typeOf e
  typeOf (Pack t _ _) = t
  typeOf (Array a _) = ArrayT $ typeOf a

instance HasAtom Obj where
  atom = traversalVL $ \f -> \case
    Fun xs e -> Fun xs <$> traverseOf atom f e
    Pack ty con xs -> Pack ty con <$> traverseOf (traversed % atom) f xs
    Array a n -> Array <$> traverseOf atom f a <*> traverseOf atom f n

{-
Programs  prog ::= f_1 = obj_1; ...; f_n = obj_n
-}
data Program a = Program
  { -- | トップレベル関数。topBinds以外の自由変数を持たない
    topFuncs :: [(a, ([a], Exp a))],
    mainExp :: Exp a
  }
  deriving stock (Eq, Show, Functor)

instance Pretty a => Pretty (Program a) where
  pPrint Program {mainExp, topFuncs} =
    parens ("entry" $$ pPrint mainExp)
      $$ vcat
        ( map
            (\(f, (ps, e)) -> parens $ "define" <+> pPrint f <+> parens (sep $ map pPrint ps) $$ pPrint e)
            topFuncs
        )

appObj :: Traversal' (Obj a) (Exp a)
appObj = traversalVL $ \f -> \case
  Fun ps e -> Fun ps <$> f e
  o -> pure o

appCase :: Traversal' (Case a) (Exp a)
appCase = traversalVL $ \f -> \case
  Unpack con ps e -> Unpack con ps <$> f e
  Switch u e -> Switch u <$> f e
  Bind x e -> Bind x <$> f e

appProgram :: Traversal' (Program a) (Exp a)
appProgram = traversalVL $ \f Program {mainExp, topFuncs} ->
  Program <$> traverseOf (traversed % _2 % _2) f topFuncs <*> f mainExp

newtype DefBuilderT m a = DefBuilderT {unDefBuilderT :: WriterT (Endo (Exp (Id Type))) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadUniq, MonadIO, MonadTrans, MonadState s, MonadReader r)

runDef :: Functor f => DefBuilderT f (Exp (Id Type)) -> f (Exp (Id Type))
runDef m = uncurry (flip appEndo) <$> runWriterT (unDefBuilderT m)

let_ :: MonadUniq m => Type -> Obj (Id Type) -> DefBuilderT m (Atom (Id Type))
let_ otype obj = do
  x <- newId otype "$let"
  DefBuilderT $ tell $ Endo $ \e -> Let [(x, obj)] e
  pure (Var x)

destruct :: MonadUniq m => Exp (Id Type) -> Con -> DefBuilderT m [Atom (Id Type)]
destruct val con@(Con _ ts) = do
  vs <- traverse (newId ?? "$p") ts
  DefBuilderT $ tell $ Endo $ \e -> Match val (Unpack con vs e :| [])
  pure $ map Var vs

bind :: MonadUniq m => Exp (Id Type) -> DefBuilderT m (Atom (Id Type))
bind (Atom a) = pure a
bind v = do
  x <- newId (typeOf v) "$d"
  DefBuilderT $ tell $ Endo $ \e -> Match v (Bind x e :| [])
  pure (Var x)

cast :: MonadUniq m => Type -> Exp (Id Type) -> DefBuilderT m (Atom (Id Type))
cast ty e
  | ty == typeOf e = bind e
  | otherwise = do
    v <- bind e
    x <- newId ty "$cast"
    DefBuilderT $ tell $ Endo $ \e -> Match (Cast ty v) (Bind x e :| [])
    pure (Var x)
