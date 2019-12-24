{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Language.Malgo.IR.HIR where

import           Language.Malgo.MiddleEnd.FreeVars
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.Prelude
import           Relude.Unsafe                  ( (!!) )

data Expr t a = Var a
              | Lit Lit
              | Tuple [a]
              | TupleAccess a Int
              | MakeArray
                t -- type of element
                a -- size
              | ArrayRead
                a -- array
                a -- index
              | ArrayWrite
                a -- array
                a -- index
                a -- value
              | Call a [a]
              | Let a (Expr t a) (Expr t a)
              | LetRec [Def t a] (Expr t a)
              | If a (Expr t a) (Expr t a)
              | Prim Text t [a]
              | BinOp Op a a
  deriving (Eq, Show, Read, Generic, Functor, Foldable)

data Def t a = Def { name :: a, params :: [a], expr :: Expr t a }
  deriving (Eq, Show, Read, Generic, Functor, Foldable)

data Lit = Int Integer
         | Float Double
         | Bool Bool
         | Char Char
         | String Text
  deriving (Eq, Show, Read, Generic)

data Op = Add | Sub | Mul | Div | Mod
        | FAdd | FSub | FMul | FDiv
        | Eq | Neq | Lt | Gt | Le | Ge
        | And | Or
  deriving (Eq, Show, Read, Generic)

flattenExpr :: Expr t a -> Expr t a
flattenExpr (Let x v1 e1) = go (flattenExpr v1)
 where
  go (Let y v2 e2) = Let y v2 (go e2)
  go (LetRec xs e) = LetRec xs (go e)
  go v             = Let x v (flattenExpr e1)
flattenExpr (LetRec defs body) =
  LetRec (map flattenDef defs) (flattenExpr body)
flattenExpr (If c t f) = If c (flattenExpr t) (flattenExpr f)
flattenExpr e          = e

flattenDef :: Def t a -> Def t a
flattenDef Def { name, params, expr } =
  Def { name = name, params = params, expr = flattenExpr expr }

instance FreeVars (Expr t) where
  freevars (Var x)            = one x
  freevars Lit{}              = mempty
  freevars (Tuple xs        ) = fromList xs
  freevars (TupleAccess x _ ) = one x
  freevars (MakeArray   _ x ) = one x
  freevars (ArrayRead   x y ) = fromList [x, y]
  freevars (ArrayWrite x y z) = fromList [x, y, z]
  freevars (Call _ xs       ) = fromList xs
  freevars (Let x v e       ) = freevars v <> delete x (freevars e)
  freevars (LetRec xs e) =
    let
      efv = freevars e
      xsfv =
        foldMap (\Def { params, expr } -> freevars expr \\ fromList params) xs
      fs = fromList $ map name xs
    in
      (efv <> xsfv) \\ fs
  freevars (If    c t f ) = one c <> freevars t <> freevars f
  freevars (Prim  _ _ xs) = fromList xs
  freevars (BinOp _ x y ) = fromList [x, y]

instance (HasType t, HasType a) => HasType (Expr t a) where
  typeOf (Var   x        ) = typeOf x
  typeOf (Lit   x        ) = typeOf x
  typeOf (Tuple xs       ) = TyApp TupleC $ map typeOf xs
  typeOf (TupleAccess x i) = case typeOf x of
    TyApp TupleC xs -> xs !! i
    _               -> error "(typeOf e) should match (TyTuple xs)"
  typeOf (MakeArray t   _) = TyApp ArrayC [typeOf t]
  typeOf (ArrayRead arr _) = case typeOf arr of
    TyApp ArrayC [t] -> t
    _                -> error "(typeOf arr) should match (TyArray xs)"
  typeOf ArrayWrite{} = TyApp TupleC []
  typeOf (Call fn _)  = case typeOf fn of
    TyApp FunC (ret : _) -> ret
    _                    -> error "(typeOf fn) should match (TyFun _ ty)"
  typeOf (Let _ _ e  ) = typeOf e
  typeOf (LetRec _ e ) = typeOf e
  typeOf (If   _ x  _) = typeOf x
  typeOf (Prim _ ty _) = case typeOf ty of
    TyApp FunC (ret : _) -> ret
    _                    -> error "(typeOf ty) should match (TyFun _ ret)"
  typeOf (BinOp op _ _) = case op of
    Add  -> TyApp IntC []
    Sub  -> TyApp IntC []
    Mul  -> TyApp IntC []
    Div  -> TyApp IntC []
    Mod  -> TyApp IntC []
    FAdd -> TyApp FloatC []
    FSub -> TyApp FloatC []
    FMul -> TyApp FloatC []
    FDiv -> TyApp FloatC []
    Eq   -> TyApp BoolC []
    Neq  -> TyApp BoolC []
    Lt   -> TyApp BoolC []
    Gt   -> TyApp BoolC []
    Le   -> TyApp BoolC []
    Ge   -> TyApp BoolC []
    And  -> TyApp BoolC []
    Or   -> TyApp BoolC []

instance HasType Lit where
  typeOf Int{}    = TyApp IntC []
  typeOf Float{}  = TyApp FloatC []
  typeOf Bool{}   = TyApp BoolC []
  typeOf Char{}   = TyApp CharC []
  typeOf String{} = TyApp StringC []

instance (Pretty t, Pretty a) => Pretty (Expr t a) where
  pPrint (Var   x             ) = pPrint x
  pPrint (Lit   x             ) = pPrint x
  pPrint (Tuple xs            ) = braces $ sep $ punctuate "," $ map pPrint xs
  pPrint (TupleAccess e   i   ) = parens $ "." <+> pPrint e <+> pPrint i
  pPrint (MakeArray   ty  size) = parens $ "array" <+> pPrint ty <+> pPrint size
  pPrint (ArrayRead   arr ix  ) = pPrint arr <> brackets (pPrint ix)
  pPrint (ArrayWrite arr ix val) =
    parens $ "<-" <+> (pPrint arr <> brackets (pPrint ix)) <+> pPrint val
  pPrint (Call f xs) = parens $ pPrint f <+> sep (map pPrint xs)
  pPrint (Let x v e) =
    parens $ "let" <+> pPrint x <+> pPrint v $+$ nest 2 (pPrint e)
  pPrint (LetRec xs e) =
    parens $ "let rec" <+> parens (sep $ map pPrint xs) $+$ nest 2 (pPrint e)
  pPrint (If c t f) =
    parens $ "if" <+> pPrint c $+$ nest 2 (pPrint t) $+$ nest 2 (pPrint f)
  pPrint (BinOp op x y) = parens $ sep [pPrint op, pPrint x, pPrint y]
  pPrint (Prim x t xs) =
    parens $ "prim" <+> pPrint x <+> pPrint t <+> sep (map pPrint xs)

instance (Pretty t, Pretty a) => Pretty (Def t a) where
  pPrint Def { name, params, expr } =
    pPrint name
      <+> parens (sep $ punctuate "," $ map pPrint params)
      <+> "="
      $$  nest 1 (pPrint expr)

instance Pretty Lit where
  pPrint (Int    x    ) = pPrint x
  pPrint (Float  x    ) = pPrint x
  pPrint (Bool   True ) = "true"
  pPrint (Bool   False) = "false"
  pPrint (Char   x    ) = quotes $ pPrint x
  pPrint (String x    ) = doubleQuotes $ pPrint x

instance Pretty Op where
  pPrint Add  = "+"
  pPrint Sub  = "-"
  pPrint Mul  = "*"
  pPrint Div  = "/"
  pPrint FAdd = "+."
  pPrint FSub = "-."
  pPrint FMul = "*."
  pPrint FDiv = "/."
  pPrint Mod  = "%"
  pPrint Eq   = "=="
  pPrint Neq  = "<>"
  pPrint Lt   = "<"
  pPrint Gt   = ">"
  pPrint Le   = "<="
  pPrint Ge   = ">="
  pPrint And  = "&&"
  pPrint Or   = "||"
