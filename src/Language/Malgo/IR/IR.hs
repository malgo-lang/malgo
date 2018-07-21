{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module Language.Malgo.IR.IR where

import           Control.Monad.Except
import           Language.Malgo.ID
import           Lens.Micro.Platform            (_1, _3)
import           RIO
import           RIO.List                       (delete, nub, (\\))
import qualified RIO.Text                       as Text
import           Text.PrettyPrint.HughesPJClass hiding ((<>))

class FreeVars f where
  freevars :: Ord a => f a -> [a]

  fv :: Ord a => f a -> [a]
  fv x = nub (freevars x)

data Program a = Program a [Defn a]
  deriving (Show, Eq, Read)

flattenProgram :: Program a -> Program a
flattenProgram (Program m defs) = Program m (map flattenDefn defs)

instance FreeVars Program where
  freevars (Program _ xs) = concatMap fv xs

instance Pretty a => Pretty (Program a) where
  pPrint (Program _ defns) =
    sep (map pPrint defns)

data Defn a = DefFun { _fnName   :: a
                     , _fnParams :: [a]
                     , _fnBody   :: Expr a
                     }
  deriving (Show, Eq, Read)

flattenDefn :: Defn a -> Defn a
flattenDefn (DefFun f params body) = DefFun f params (flattenExpr body)

instance FreeVars Defn where
  freevars (DefFun _ params body) =
    fv body \\ params

instance Pretty a => Pretty (Defn a) where
  pPrint (DefFun fn params body) =
    parens ("define" <+> parens (pPrint fn <+> parens (sep (map pPrint params))) $$ nest 2 (pPrint body))

{- Closure representation

Tuple [fn :: FunctionTy ret [PointerTy (IntTy 8), x, y, ...], Cast (PointerTy (IntTy 8)) (Tuple ..)]

cls = Tuple [fn, env]
fn = Access cls [0, 0]
env = Access cls [0, 1]
Apply fn (env : args)
-}

data Expr a = Var a
            | Int Integer
            | Float Double
            | Bool Bool
            | Char Char
            | String Text
            | Unit
            | Prim Text MType
            | Tuple [a]
            | Apply a [a]
            | Let a (Expr a) (Expr a)
            | LetRec [(a, Maybe [a], Expr a)] (Expr a)
            | Cast MType a
            | Access a [Int]
            | If a (Expr a) (Expr a)
  deriving (Show, Eq, Read, Functor, Foldable, Traversable)

flattenExpr :: Expr a -> Expr a
flattenExpr (Let x v1 e1) =
  insert (flattenExpr v1)
  where insert (Let y v2 e2) = Let y v2 (insert e2)
        insert (LetRec xs e) = LetRec xs (insert e)
        insert v             = Let x v (flattenExpr e1)
flattenExpr (LetRec defs body) =
  LetRec (map flattenDef defs) (flattenExpr body)
  where flattenDef (f, p, e) = (f, p, flattenExpr e)
flattenExpr (If c t f) = If c (flattenExpr t) (flattenExpr f)
flattenExpr e = e

prims :: Expr a -> [Expr a]
prims p@Prim{} = [p]
prims (Let _ v e) = prims v ++ prims e
prims (LetRec vs e) = prims' ++ prims e
  where prims' = concatMap (prims . view _3) vs
prims (If _ t f) = prims t ++ prims f
prims _ = []

instance FreeVars Expr where
  freevars (Var x) = [x]
  freevars (Tuple xs) = xs
  freevars (Apply _ args) = args
  freevars (Let x v e) = fv v ++ delete x (fv e)
  freevars (LetRec xs e) =
    (concatMap (\(_, params, body) -> (fv body \\ fromMaybe [] params)) xs ++ fv e)
    \\ map (view _1) xs
  freevars (Cast _ x) = [x]
  freevars (Access x _) = [x]
  freevars (If c t f) = c : fv t ++ fv f
  freevars _ = []

instance Pretty a => Pretty (Expr a) where
  pPrint (Var a) = pPrint a
  pPrint (Int i) = pPrint i
  pPrint (Float d) = pPrint d
  pPrint (Bool True) = "true"
  pPrint (Bool False) = "false"
  pPrint (Char c) = quotes $ pPrint c
  pPrint (String s) = doubleQuotes $ pPrint (Text.unpack s)
  pPrint Unit = lparen <> rparen
  pPrint (Prim name _) = "#" <> text (Text.unpack name)
  pPrint (Tuple xs) = "tuple" <> parens (sep $ punctuate "," $ map pPrint xs)
  pPrint (Apply f args) = parens (pPrint f <+> sep (map pPrint args))
  pPrint (Let name val body) =
    parens ("let" <+> parens (pPrint name <+> pPrint val)
             $$ nest 1 (pPrint body))
  pPrint (LetRec defs body) =
    parens ("let" <+> sep (map (\(name, params, val) ->
                                          parens ("rec" <+> pPrint name <+> sep (map pPrint (fromMaybe [] params))
                                                  $$ nest 1 (pPrint val))) defs)
             $$ nest 1 (pPrint body))
  pPrint (Cast ty val) = parens ("cast" <+> pPrint ty <+> pPrint val)
  pPrint (Access e is) = parens ("access" <+> pPrint e <+> sep (map pPrint is))
  pPrint (If c t f) =
    parens ("if" <+> (pPrint c $+$ pPrint t $+$ pPrint f))

instance HasMType a => HasMType (Expr a) where
  mTypeOf (Var a) = mTypeOf a
  mTypeOf (Int _) = IntTy 32
  mTypeOf (Float _) = DoubleTy
  mTypeOf (Bool _) = IntTy 1
  mTypeOf (Char _) = IntTy 8
  mTypeOf (String _) = PointerTy (IntTy 8)
  mTypeOf Unit = StructTy []
  mTypeOf (Prim _ ty) = ty
  mTypeOf (Tuple xs) = PointerTy (StructTy (map mTypeOf xs))
  mTypeOf (Apply f _) =
    case mTypeOf f of
      FunctionTy t _ -> t -- normal function
      -- PointerTy (StructTy [FunctionTy t _, _]) -> t -- closure
      t              -> error $ show $ pPrint t <+> "is not applieable"
  mTypeOf (Let _ _ body) = mTypeOf body
  mTypeOf (LetRec _ body) = mTypeOf body
  mTypeOf (Cast ty _) = ty
  mTypeOf (Access e is) =
    case runExcept (accessMType (mTypeOf e) is) of
      Right t  -> t
      Left mes -> error $ show mes
  mTypeOf (If _ e _) = mTypeOf e

data MType = IntTy Integer
           | DoubleTy
           | PointerTy MType
           | StructTy [MType]
           | FunctionTy MType [MType]
  deriving (Show, Eq, Read, Ord)

class HasMType a where
  mTypeOf :: a -> MType

instance HasMType MType where
  mTypeOf = id

instance HasMType a => HasMType (ID a) where
  mTypeOf (ID _ _ m) = mTypeOf m

instance Pretty MType where
  pPrint (IntTy i) = "i" <> pPrint i
  pPrint DoubleTy = "double"
  pPrint (PointerTy t) = parens $ "ptr" <+> pPrint t
  pPrint (StructTy ts) = parens $ "struct" <+> parens (sep (punctuate "," $ map pPrint ts))
  pPrint (FunctionTy ret params) =
    parens $ "fun" <+> pPrint ret
    <+> parens (sep (punctuate "," $ map pPrint params))

accessMType :: MonadError Doc m => MType -> [Int] -> m MType
accessMType x [] = return x
accessMType (PointerTy x) (_:is) = accessMType x is
accessMType t@(StructTy xs) (i:is) =
  case atMay xs i of
    Just xt -> accessMType xt is
    Nothing -> throwError $ "out of bounds:" <+> (pPrint t <> ",") <+> pPrint i
  where atMay (y:_) 0  = Just y
        atMay [] _     = Nothing
        atMay (_:ys) n = atMay ys (n - 1)
accessMType t _ = throwError $ pPrint t <+> "is not accessable MType"
