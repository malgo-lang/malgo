{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Language.Malgo.IR.MIR where

import           Language.Malgo.IR.HIR          ( Lit(..)
                                                , Op(..)
                                                )
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.Prelude
import           Relude.Unsafe                  ( (!!) )
import           Text.PrettyPrint.HughesPJClass ( vcat
                                                , ($$)
                                                , ($+$)
                                                , nest
                                                , brackets
                                                , sep
                                                , braces
                                                , punctuate
                                                , parens
                                                )

data Program a = Program { functions :: [Func a], mainExpr :: Expr a }
  deriving (Eq, Show, Read, Generic, Functor, Foldable)

instance Pretty a => Pretty (Program a) where
  pPrint Program { functions, mainExpr } =
    vcat (map pPrint functions) $$ "entry point =" $+$ nest 2 (pPrint mainExpr)

data Func a = Func { name :: a, captures :: Maybe [a], mutrecs :: [a], params :: [a], body :: Expr a }
  deriving (Eq, Show, Read, Generic, Functor, Foldable)

instance Pretty a => Pretty (Func a) where
  pPrint Func { name, captures, params, body } = case captures of
    Just caps ->
      pPrint name <+> brackets (sep $ map pPrint caps) <+> sep (map pPrint params) <+> "=" $+$ nest
        2
        (pPrint body)
    Nothing -> pPrint name <+> sep (map pPrint params) <+> "=" $+$ nest 2 (pPrint body)

data Expr a = Var a
            | Lit Lit
            | Tuple [a]
            | TupleAccess a Int
            | MakeArray
              a -- init
              a -- size
            | ArrayRead
              a -- array
              a -- index
            | ArrayWrite
              a -- array
              a -- index
              a -- value
            | MakeClosure
              a -- function
              [a] -- captured variables (may recursive)
            | CallDirect a [a] -- direct call
            | CallWithCaptures a [a] -- indirect call for mutrec functions
            | CallClosure a [a] -- indirect call
            | Let a (Expr a) (Expr a)
            | If a (Expr a) (Expr a)
            | Prim String Type [a]
            | BinOp Op a a
  deriving (Eq, Show, Read, Generic, Functor, Foldable)

flattenExpr :: Expr a -> Expr a
flattenExpr (Let x v1 e1) = go (flattenExpr v1)
 where
  go (Let y v2 e2) = Let y v2 (go e2)
  go v             = Let x v (flattenExpr e1)
flattenExpr (If c t f) = If c (flattenExpr t) (flattenExpr f)
flattenExpr e          = e

flattenDef :: (a, [a], Expr a) -> (a, [a], Expr a)
flattenDef (f, ps, e) = (f, ps, flattenExpr e)

instance Pretty a => Pretty (Expr a) where
  pPrint (Var   x             ) = pPrint x
  pPrint (Lit   x             ) = pPrint x
  pPrint (Tuple xs            ) = braces $ sep $ punctuate "," $ map pPrint xs
  pPrint (TupleAccess e   i   ) = parens $ "." <+> pPrint e <+> pPrint i
  pPrint (MakeArray   ty  size) = parens $ "array" <+> pPrint ty <+> pPrint size
  pPrint (ArrayRead   arr ix  ) = pPrint arr <> brackets (pPrint ix)
  pPrint (ArrayWrite arr ix val) =
    parens $ "<-" <+> (pPrint arr <> brackets (pPrint ix)) <+> pPrint val
  pPrint (CallDirect       f xs) = parens $ "dir" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (CallWithCaptures f xs) = parens $ "withcap" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (CallClosure      f xs) = parens $ "cls" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (MakeClosure      f xs) = parens $ "closure" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (Let   n  v e         ) = pPrint n <+> "=" <+> pPrint v $+$ pPrint e
  pPrint (If c t f) = parens $ "if" <+> pPrint c $+$ "then" <+> pPrint t $+$ "else" <+> pPrint f
  pPrint (BinOp op x y         ) = parens $ sep [pPrint op, pPrint x, pPrint y]
  pPrint (Prim  x  t xs        ) = parens $ "prim" <+> pPrint x <+> pPrint t <+> sep (map pPrint xs)

instance HasType a => HasType (Expr a) where
  typeOf (Var   x        ) = typeOf x
  typeOf (Lit   x        ) = typeOf x
  typeOf (Tuple xs       ) = TyApp TupleC $ map typeOf xs
  typeOf (TupleAccess x i) = case typeOf x of
    TyApp TupleC xs -> xs !! i
    _               -> error "(typeOf e) should match (TyTuple xs)"
  typeOf (MakeArray ty  _) = TyApp ArrayC [typeOf ty]
  typeOf (ArrayRead arr _) = case typeOf arr of
    TyApp ArrayC [ty] -> ty
    _                 -> error "(typeOf arr) should match (TyArray xs)"

  typeOf ArrayWrite{}      = TyApp TupleC []
  typeOf (CallDirect fn _) = case typeOf fn of
    TyApp FunC (ret : _) -> ret
    _                    -> error "(typeOf fn) should match (TyFun _ ty)"
  typeOf (CallWithCaptures fn _) = case typeOf fn of
    TyApp FunC (ret : _) -> ret
    _                    -> error "(typeOf fn) should match (TyFun _ ty)"
  typeOf (CallClosure fn _) = case typeOf fn of
    TyApp FunC (ret : _) -> ret
    _                    -> error "(typeOf fn) should match (TyFun _ ty)"
  typeOf (MakeClosure fn _) = TyApp TupleC [typeOf fn, TyApp StringC []]
  typeOf (Let  _ _  e     ) = typeOf e
  typeOf (If   _ t  _     ) = typeOf t
  typeOf (Prim _ ty _     ) = case typeOf ty of
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
