{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.IR.Core where

import Data.Set.Lens
import Data.Text (unpack)
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.CType
import Text.PrettyPrint.HughesPJ
  ( ($$),
    brackets,
    char,
    doubleQuotes,
    nest,
    parens,
    quotes,
    sep,
    text,
    vcat,
  )

class HasFreeVar e a | e -> a where
  freevars :: e -> Set a

{-
Atoms  a ::= unboxed | x
-}
data Atom a
  = Var a
  | Unboxed Unboxed
  deriving stock (Eq, Show, Functor)

instance HasCType a => HasCType (Atom a) where
  cTypeOf (Var x) = cTypeOf x
  cTypeOf (Unboxed x) = cTypeOf x

instance Pretty a => Pretty (Atom a) where
  pPrint (Var x) = pPrint x
  pPrint (Unboxed x) = pPrint x

instance Ord a => HasFreeVar (Atom a) a where
  freevars (Var x) = setOf id x
  freevars Unboxed {} = mempty

{-
Unboxed values  unboxed
-}
data Unboxed
  = Int Integer
  | Float Double
  | Char Char
  | String String
  deriving stock (Eq, Ord, Show)

instance HasCType Unboxed where
  cTypeOf Int {} = IntT
  cTypeOf Float {} = FloatT
  cTypeOf Char {} = CharT
  cTypeOf String {} = StringT

instance Pretty Unboxed where
  pPrint (Int x) = pPrint x
  pPrint (Float x) = pPrint x
  pPrint (Char x) = quotes (char x)
  pPrint (String x) = doubleQuotes (text x)

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
  | Call a [Atom a]
  | PrimCall Text CType [Atom a]
  | ArrayRead (Atom a) (Atom a)
  | ArrayWrite (Atom a) (Atom a) (Atom a)
  | Let [(a, Obj a)] (Exp a)
  | Match (Exp a) (NonEmpty (Case a))
  deriving stock (Eq, Show, Functor)

instance HasCType a => HasCType (Exp a) where
  cTypeOf (Atom x) = cTypeOf x
  cTypeOf (Call f xs) = returnType (cTypeOf f) xs
  cTypeOf (PrimCall _ t xs) = returnType t xs
  cTypeOf (ArrayRead a _) = case cTypeOf a of
    ArrayT t -> t
    _ -> bug Unreachable
  cTypeOf ArrayWrite {} = PackT [Con "Tuple0" []]
  cTypeOf (Let _ e) = cTypeOf e
  cTypeOf (Match _ (Unpack _ _ e :| _)) = cTypeOf e
  cTypeOf (Match _ (Bind _ e :| _)) = cTypeOf e

returnType :: CType -> [a] -> CType
returnType t [] = t
returnType (_ :-> t) [_] = t
returnType (_ :-> t) (_ : rest) = returnType t rest
returnType AnyT _ = AnyT
returnType _ _ = bug Unreachable

instance Pretty a => Pretty (Exp a) where
  pPrint (Atom x) = pPrint x
  pPrint (Call f xs) = parens $ pPrint f <+> sep (map pPrint xs)
  pPrint (PrimCall p _ xs) = parens $ text (unpack p) <+> sep (map pPrint xs)
  pPrint (ArrayRead a b) = pPrint a <> brackets (pPrint b)
  pPrint (ArrayWrite a b c) = parens $ pPrint a <> brackets (pPrint b) <+> "<-" <+> pPrint c
  pPrint (Let xs e) = parens $ "let" <+> vcat (map (\(v, o) -> parens $ pPrint v <+> "=" <+> pPrint o) xs) $$ pPrint e
  pPrint (Match v cs) = parens $ "match" <+> pPrint v $$ nest 2 (vcat (toList $ fmap pPrint cs))

instance Ord a => HasFreeVar (Exp a) a where
  freevars (Atom x) = freevars x
  freevars (Call f xs) = foldMap freevars xs <> setOf id f
  freevars (PrimCall _ _ xs) = foldMap freevars xs
  freevars (ArrayRead a b) = freevars a <> freevars b
  freevars (ArrayWrite a b c) = freevars a <> freevars b <> freevars c
  freevars (Let xs e) = foldr (sans . view _1) (freevars e <> foldMap (freevars . view _2) xs) xs
  freevars (Match e cs) = freevars e <> foldMap freevars cs

{-
Alternatives  alt ::= UNPACK(C x_1 ... x_n) -> e  (n >= 0)
                    | BIND x -> e
-}
data Case a
  = Unpack Con [a] (Exp a)
  | Bind a (Exp a)
  deriving stock (Eq, Show, Functor)

instance Pretty a => Pretty (Case a) where
  pPrint (Unpack c xs e) = parens $ "unpack" <> parens (pPrint c <+> sep (map pPrint xs)) <+> "->" $$ nest 2 (pPrint e)
  pPrint (Bind x e) = parens $ "bind" <+> pPrint x <+> "->" $$ nest 2 (pPrint e)

instance Ord a => HasFreeVar (Case a) a where
  freevars (Unpack _ xs e) = foldr sans (freevars e) xs
  freevars (Bind x e) = sans x $ freevars e

{-
Heap objects  obj ::= FUN(x_1 ... x_n -> e)  Function (arity = n >= 1)
                    | PAP(f a_1 ... a_n)     Partial application (f is always a FUN with arity(f) > n >= 1)
                    | PACK(C a_1 ... a_n)    Saturated constructor (n >= 0)
                    | ARRAY(a, n)            Array (n > 0)
-}
data Obj a
  = Fun [a] (Exp a)
  | Pack Con [Atom a]
  | Array (Atom a) (Atom a)
  deriving stock (Eq, Show, Functor)

instance Pretty a => Pretty (Obj a) where
  pPrint (Fun xs e) = "fun" <> parens (sep (map pPrint xs) <+> "->" $$ nest 2 (pPrint e))
  pPrint (Pack c xs) = "pack" <> parens (pPrint c <+> sep (map pPrint xs))
  pPrint (Array a n) = "array" <> parens (pPrint a <> "," <+> pPrint n)

instance Ord a => HasFreeVar (Obj a) a where
  freevars (Fun as e) = foldr sans (freevars e) as
  freevars (Pack _ xs) = foldMap freevars xs
  freevars (Array a n) = freevars a <> freevars n

{-
Programs  prog ::= f_1 = obj_1; ...; f_n = obj_n
-}
data Program a = Program a [(a, Obj a)]
  deriving stock (Eq, Show, Functor)

instance Pretty a => Pretty (Program a) where
  pPrint (Program _ xs) = vcat $ map (\(f, o) -> pPrint f <+> "=" <+> pPrint o <> ";") xs
-- Examples

-- plusInt :: Obj Text
-- plusInt =
--   Fun ["a", "b"] $
--     Match
--       (Atom (Var "a"))
--       [ Unpack (Con "Int" [IntT]) ["x"] $
--           Match
--             (Atom (Var "b"))
--             [ Unpack (Con "Int" [IntT]) ["y"] $
--                 Match
--                   (PrimCall "+" (IntT :-> IntT :-> IntT) [Var "x", Var "y"])
--                   [Bind "z" $ Let [("r", Pack (Con "Int" [IntT]) [Var "z"])] $ Atom (Var "r")]
--             ]
--       ]

-- fib :: Obj Text
-- fib =
--   Fun ["n"] $
--     Match
--       (Atom (Var "n"))
--       [ Unpack (Con "Int" [IntT]) ["n'"] $
--           Match
--             (PrimCall "<=" (IntT :-> IntT :-> PackT [Con "True" [], Con "False" []]) [Var "n'", Unboxed (Int 1)])
--             [ Unpack (Con "False" []) [] $ Let [("v1", Pack (Con "Int" [IntT]) [Unboxed (Int 1)])] $
--                 Match
--                   (Call "minusInt" [Var "n", Var "v1"])
--                   [ Bind "n2" $
--                       Match
--                         (Call "fib" [Var "n2"])
--                         [ Bind "v2" $ Let [("v3", Pack (Con "Int" [IntT]) [Unboxed (Int 1)])] $
--                             Match
--                               (Call "minusInt" [Var "n", Var "v3"])
--                               [Bind "n3" $ Match (Call "fib" [Var "n3"]) [Bind "v4" $ Call "plusInt" [Var "v2", Var "v4"]]]
--                         ]
--                   ],
--               Unpack (Con "True" []) [] $ Let [("v5", Pack (Con "Int" [IntT]) [Unboxed (Int 1)])] (Atom (Var "v5"))
--             ]
--       ]
