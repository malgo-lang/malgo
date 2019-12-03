{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Language.Malgo.IR.LIR where

import           Data.Set              (delete)
import           Language.Malgo.Pretty
import           Relude                hiding (Op)

type Program t a = ([Func t a], Expr t a)

data Func t a = Func { name :: a, captures :: [a], params :: [a], body :: Expr t a }
  deriving (Eq, Show, Read, Generic, PrettyVal, Functor, Foldable)

instance (Pretty t, Pretty a) => Pretty (Func t a) where
  pPrint Func { name, captures, params, body } =
    pPrint name <+> brackets (sep $ map pPrint captures) <+> sep (map pPrint params) <+> "="
    $+$ pPrint body

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
              | MakeCls
                a -- function
                [a] -- captured variables
              | CallDir a [a]
              | CallCls a [a]
              | Let a (Expr t a) (Expr t a)
              | If a (Expr t a) (Expr t a)
              | Prim Text t
              | BinOp Op a a
  deriving (Eq, Show, Read, Generic, PrettyVal, Functor, Foldable)

data Lit = Int Integer
         | Float Double
         | Bool Bool
         | Char Char
         | String Text
  deriving (Eq, Show, Read, Generic, PrettyVal)

data Op = Add | Sub | Mul | Div | Mod
        | FAdd | FSub | FMul | FDiv
        | Eq | Neq | Lt | Gt | Le | Ge
        | And | Or
  deriving (Eq, Show, Read, Generic, PrettyVal)

flattenExpr :: Expr t a -> Expr t a
flattenExpr (Let x v1 e1) =
  insert (flattenExpr v1)
  where insert (Let y v2 e2) = Let y v2 (insert e2)
        insert v             = Let x v (flattenExpr e1)
flattenExpr (If c t f) = If c (flattenExpr t) (flattenExpr f)
flattenExpr e = e

flattenDef :: (a, [a], Expr t a) -> (a, [a], Expr t a)
flattenDef (f, ps, e) = (f, ps, flattenExpr e)

freevars :: Ord a => Expr t a -> Set a
freevars (Var x)            = one x
freevars Lit{}              = mempty
freevars (Tuple xs)         = fromList xs
freevars (TupleAccess x _)  = one x
freevars (MakeArray _ x)    = one x
freevars (ArrayRead x y)    = fromList [x, y]
freevars (ArrayWrite x y z) = fromList [x, y, z]
freevars (CallDir x xs)     = fromList $ x:xs
freevars (CallCls x xs)     = fromList $ x:xs
freevars (MakeCls x xs)     = fromList $ x:xs
freevars (Let x v e)        = freevars v <> delete x (freevars e)
freevars (If c t f)         = one c <> freevars t <> freevars f
freevars (Prim _ _)         = mempty
freevars (BinOp _ x y)      = fromList [x, y]

instance (Pretty t, Pretty a) => Pretty (Expr t a) where
  pPrint (Var x) = pPrint x
  pPrint (Lit x) = pPrint x
  pPrint (Tuple xs) = braces $ sep $ punctuate "," $ map pPrint xs
  pPrint (TupleAccess e i) = parens $ "." <+> pPrint e <+> pPrint i
  pPrint (MakeArray ty size) = parens $ "array" <+> pPrint ty <+> pPrint size
  pPrint (ArrayRead arr ix) = pPrint arr <> brackets (pPrint ix)
  pPrint (ArrayWrite arr ix val) = parens $ "<-" <+> (pPrint arr <> brackets (pPrint ix)) <+> pPrint val
  pPrint (CallDir f xs) = parens $ "dir" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (CallCls f xs) = parens $ "cls" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (MakeCls f xs) = parens $ "closure" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (Let x v e) =
    parens $ "let" <+> pPrint x <+> pPrint v
    $+$ nest 2 (pPrint e)
  pPrint (If c t f) =
    parens $ "if" <+> pPrint c
    $+$ nest 2 (pPrint t)
    $+$ nest 2 (pPrint f)
  pPrint (BinOp op x y) =
    parens $ sep [pPrint op, pPrint x, pPrint y]
  pPrint (Prim x t) = parens $ "prim" <+> pPrint x <+> pPrint t

instance Pretty Lit where
  pPrint (Int x)      = pPrint x
  pPrint (Float x)    = pPrint x
  pPrint (Bool True)  = "true"
  pPrint (Bool False) = "false"
  pPrint (Char x)     = quotes $ pPrint x
  pPrint (String x)   = doubleQuotes $ pPrint x

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
