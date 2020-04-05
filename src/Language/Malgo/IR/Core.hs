{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Language.Malgo.IR.Core where

import           Language.Malgo.Prelude
import           Language.Malgo.TypeRep.CType

{-
Atoms  a ::= unboxed | x
-}
data Atom a = Var a
    | Unboxed Unboxed
    deriving stock (Eq, Show, Functor)

instance HasCType a => HasCType (Atom a) where
  cTypeOf (Var x)     = cTypeOf x
  cTypeOf (Unboxed x) = cTypeOf x

{-
Unboxed values  unboxed
-}
data Unboxed = Int Integer
    | Float Double
    | Char Char
    | String String
    deriving stock (Eq, Show)

instance HasCType Unboxed where
  cTypeOf Int{}    = IntT
  cTypeOf Float{}  = FloatT
  cTypeOf Char{}   = CharT
  cTypeOf String{} = StringT

{-
Constructors  C ::= <tag n>
-}
type Tag = Text

data Con = Con Tag Int
    deriving stock (Eq, Show)

{-
Expressions  e ::= a              Atom
                 | f a_1 ... a_n  Function call (arity(f) >= 1)
                 | p a_1 ... a_n  Saturated primitive operation (n >= 1)
                 | LET x = obj IN e
                 | MATCH e WITH { alt_1; ... alt_n; x -> e } (n >= 0)
-}
data Exp a = Atom (Atom a)
    | Call a [Atom a]
    | PrimCall Text CType [Atom a]
    | Let [(a, Obj a)] (Exp a)
    | Match (Exp a) [Case a] (a, Exp a)
    | Undefined
    deriving stock (Eq, Show, Functor)

instance HasCType a => HasCType (Exp a) where
  cTypeOf (Atom x) = cTypeOf x
  cTypeOf (Call f xs) = go (cTypeOf f) xs
    where go (_ :-> t) []       = t
          go (_ :-> t) (_:rest) = go t rest
          go AnyT _             = AnyT
          go _ _                = bug Unreachable
{-
Alternatives  alt ::= UNPACK(C x_1 ... x_n) -> e  (n >= 0)
-}
data Case a = Unpack Con [a] (Exp a)
    deriving stock (Eq, Show, Functor)

{-
Heap objects  obj ::= FUN(x_1 ... x_n -> e)  Function (arity = n >= 1)
                    | PAP(f a_1 ... a_n)     Partial application (f is always a FUN with arity(f) > n >= 1)
                    | PACK(C a_1 ... a_n)    Saturated constructor (n >= 0)
-}
data Obj a = Fun [a] (Exp a)
    | Pap a [Atom a]
    | Pack Con [Atom a]
    deriving stock (Eq, Show, Functor)

{-
Programs  prog ::= f_1 = obj_1; ...; f_n = obj_n
-}
newtype Program a = Program [(a, Obj a)]
    deriving stock (Eq, Show, Functor)

-- Examples

{-
plusInt = FUN(a b ->
  MATCH a WITH { UNPACK(<Int 1> x) ->
    MATCH b WITH { UNPACK(<Int 1> y) ->
      MATCH (+) x y WITH {
        z -> LET r = PACK(<Int 1> z) IN r
      }
    }
  })
-}
plusInt :: Obj Text
plusInt = Fun ["a", "b"] $ Match
  (Atom (Var "a"))
  [ Unpack (Con "Int" 1) ["x"] $ Match
      (Atom (Var "b"))
      [ Unpack (Con "Int" 1) ["y"] $ Match
          (PrimCall "+" (IntT :-> IntT :-> IntT) [Var "x", Var "y"])
          []
          ("z", Let [("r", Pack (Con "Int" 1) [Var "z"])] $ Atom (Var "r"))
      ]
      ("_", Undefined)
  ]
  ("_", Undefined)

{-
fib = FUN(n ->
  MATCH n WITH { UNPACK(<Int 1> n') ->
    MATCH (<=) n' 1 WITH {
      UNPACK(<False 0>) ->
        LET v1 = PACK(<Int 1> 1)
        IN MATCH minusInt n v1 WITH {
             n2 ->
             MATCH fib n2 WITH {
               v2 ->
                 LET v3 = PACK(<Int 1> 2)
                 IN MATCH minusInt n v3 { n3 -> MATCH fib n3 WITH {
                      v4 -> plusInt v2 v4
                    }}
             }
           };
      UNPACK(<True 0>) -> LET v5 = PACK(<Int 1> 1) IN v5
    }
  })
-}
fib :: Obj Text
fib = Fun ["n"] $ Match
  (Atom (Var "n"))
  [ Unpack (Con "Int" 1) ["n'"] $ Match
      (PrimCall "<=" (IntT :-> IntT :-> PackT "Bool" []) [Var "n'", Unboxed (Int 1)])
      [ Unpack (Con "False" 0) [] $ Let [("v1", Pack (Con "Int" 1) [Unboxed (Int 1)])] $ Match
        (Call "minusInt" [Var "n", Var "v1"])
        []
        ( "n2"
        , Match
          (Call "fib" [Var "n2"])
          []
          ( "v2"
          , Let [("v3", Pack (Con "Int" 1) [Unboxed (Int 1)])] $ Match
            (Call "minusInt" [Var "n", Var "v3"])
            []
            ("n3", Match (Call "fib" [Var "n3"]) [] ("v4", Call "plusInt" [Var "v2", Var "v4"]))
          )
        )
      , Unpack (Con "True" 0) [] $ Let [("v5", Pack (Con "Int" 1) [Unboxed (Int 1)])] (Atom (Var "v5"))
      ]
      ("_", Undefined)
  ]
  ("_", Undefined)
