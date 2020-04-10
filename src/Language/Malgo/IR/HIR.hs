{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
module Language.Malgo.IR.HIR
  ( module Export
  , Expr(..)
  , Def(..)
  , Lit(..)
  , Pat(..)
  , flattenDef
  , flattenExpr
  )
where

import           Language.Malgo.Prelude                    hiding (ix, op)
import           Language.Malgo.Pretty

import           Language.Malgo.TypeRep.Type

import           Language.Malgo.IR.Op                      as Export

import           Language.Malgo.FrontEnd.Typing.Constraint
import           Language.Malgo.FrontEnd.Typing.Subst

import           Text.PrettyPrint.HughesPJClass            (braces, brackets,
                                                            doubleQuotes,
                                                            parens, punctuate,
                                                            quotes, sep, ($+$))

data Expr a = Var a
    | Lit Lit
    | Tuple [a]
    | TupleAccess a Int
    | MakeArray a a
    | ArrayRead a a
    | ArrayWrite a a a
    | Call a [a]
    | Let a (Expr a) (Expr a)
    | LetRec [Def a] (Expr a)
    | If a (Expr a) (Expr a)
    | Prim String Type [a]
    | BinOp Op a a
    | Match a (NonEmpty (Pat a, Expr a))
    deriving stock (Eq, Show)

data Pat a = VarP a
    | TupleP [a]
    deriving stock (Eq, Show)

data Def a = Def
    { name   :: a
    , params :: [a]
    , expr   :: Expr a
    }
    deriving stock (Eq, Show)

data Lit = Int Integer
    | Float Double
    | Bool Bool
    | Char Char
    | String String
    deriving stock (Eq, Show)

flattenExpr :: Expr a -> Expr a
flattenExpr (Let x v1 e1) = go (flattenExpr v1)
 where
  go (Let y v2 e2) = Let y v2 (go e2)
  go (LetRec xs e) = LetRec xs (go e)
  go v             = Let x v (flattenExpr e1)
flattenExpr (LetRec defs body) = LetRec (map flattenDef defs) (flattenExpr body)
flattenExpr (If c t f        ) = If c (flattenExpr t) (flattenExpr f)
flattenExpr (Match s cs      ) = Match s (fmap (second flattenExpr) cs)
flattenExpr e                  = e

flattenDef :: Def a -> Def a
flattenDef Def { name, params, expr } =
  Def { name = name, params = params, expr = flattenExpr expr }

instance HasType a => HasType (Expr a) where
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
  typeOf (Call fn as) = case typeOf fn of
    ps :-> ret -> apply (fold $ solve $ zipWith (:~) (map typeOf as) ps) ret
    _          -> error "(typeOf fn) should match (TyFun _ ty)"
  typeOf (Let _ _ e  ) = typeOf e
  typeOf (LetRec _ e ) = typeOf e
  typeOf (If   _ x  _) = typeOf x
  typeOf (Prim _ ty _) = case typeOf ty of
    _ :-> ret -> ret
    _         -> error "(typeOf ty) should match (TyFun _ ret)"
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
  typeOf (Match _ ((_, e) :| _)) = typeOf e

instance HasType Lit where
  typeOf Int{}    = TyApp IntC []
  typeOf Float{}  = TyApp FloatC []
  typeOf Bool{}   = TyApp BoolC []
  typeOf Char{}   = TyApp CharC []
  typeOf String{} = TyApp StringC []

instance Pretty a => Pretty (Expr a) where
  pPrint (Var   x           ) = pPrint x
  pPrint (Lit   x           ) = pPrint x
  pPrint (Tuple xs          ) = braces $ sep $ punctuate "," $ map pPrint xs
  pPrint (TupleAccess e   i ) = parens $ "." <+> pPrint e <+> pPrint i
  pPrint (MakeArray   x   n ) = parens $ "array" <+> pPrint x <+> pPrint n
  pPrint (ArrayRead   arr ix) = pPrint arr <> brackets (pPrint ix)
  pPrint (ArrayWrite arr ix val) =
    parens $ "<-" <+> (pPrint arr <> brackets (pPrint ix)) <+> pPrint val
  pPrint (Call f xs    ) = parens $ pPrint f <+> sep (map pPrint xs)
  pPrint (Let x v e    ) = parens $ "let" <+> pPrint x <+> pPrint v $+$ pPrint e
  pPrint (LetRec xs e  ) = parens $ "let rec" <+> parens (sep $ map pPrint xs) $+$ pPrint e
  pPrint (If    c  t f ) = parens $ "if" <+> pPrint c $+$ pPrint t $+$ pPrint f
  pPrint (Prim  x  t xs) = parens $ "prim" <+> pPrint x <+> pPrint t <+> sep (map pPrint xs)
  pPrint (BinOp op x y ) = parens $ sep [pPrint op, pPrint x, pPrint y]
  pPrint (Match s cs   ) = parens $ "match" <+> pPrint s $+$ parens (sep $ toList $ fmap f cs)
    where f (p, e) = parens $ pPrint p <+> pPrint e

instance Pretty a => Pretty (Pat a) where
  pPrint (VarP   x ) = pPrint x
  pPrint (TupleP xs) = braces $ sep $ punctuate "," $ map pPrint xs

instance Pretty a => Pretty (Def a) where
  pPrint Def { name, params, expr } =
    pPrint name <+> parens (sep $ punctuate "," $ map pPrint params) <+> "=" $+$ pPrint expr

instance Pretty Lit where
  pPrint (Int    x    ) = pPrint x
  pPrint (Float  x    ) = pPrint x
  pPrint (Bool   True ) = "true"
  pPrint (Bool   False) = "false"
  pPrint (Char   x    ) = quotes $ pPrint x
  pPrint (String x    ) = doubleQuotes $ pPrint x
