{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module Language.Malgo.IR.IR where

import           Control.Monad.Except
import           Data.Text.Prettyprint.Doc
import           Language.Malgo.ID
import           Lens.Micro.Platform       (_1, _3)
import           RIO
import           RIO.List                  (delete, nub, (\\))
import qualified RIO.Text                  as Text

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
  pretty (Program _ defns) =
    sep (map pretty defns)

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
  pretty (DefFun fn params body) =
    parens ("define" <+> parens (pretty fn <+> parens (sep (map pretty params))) <> line <> indent 2 (pretty body))

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
  pretty (Var a) = pretty a
  pretty (Int i) = pretty i
  pretty (Float d) = pretty d
  pretty (Bool True) = "true"
  pretty (Bool False) = "false"
  pretty (Char c) = squotes $ pretty c
  pretty (String s) = dquotes $ pretty s
  pretty Unit = lparen <> rparen
  pretty (Prim name _) = "#" <> pretty name
  pretty (Tuple xs) = "tuple" <> parens (sep $ punctuate "," $ map pretty xs)
  pretty (Apply f args) = parens (pretty f <+> sep (map pretty args))
  pretty (Let name val body) =
    parens ("let" <+> parens (pretty name <+> pretty val)
             <> line <> indent 1 (pretty body))
  pretty (LetRec defs body) =
    parens ("let" <+> sep (map (\(name, params, val) ->
                                          parens ("rec" <+> pretty name <+> sep (map pretty (fromMaybe [] params))
                                                  <> line <> indent 1 (pretty val))) defs)
             <> line <> indent 1 (pretty body))
  pretty (Cast ty val) = parens ("cast" <+> pretty ty <+> pretty val)
  pretty (Access e is) = parens ("access" <+> pretty e <+> sep (map pretty is))
  pretty (If c t f) =
    parens ("if" <+> (pretty c <> line <> pretty t <> line <> pretty f))

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
      t              -> error $ show $ pretty t <+> "is not applieable"
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
  pretty (IntTy i) = "i" <> pretty i
  pretty DoubleTy = "double"
  pretty (PointerTy t) = parens $ "ptr" <+> pretty t
  pretty (StructTy ts) = parens $ "struct" <+> parens (sep (punctuate "," $ map pretty ts))
  pretty (FunctionTy ret params) =
    parens $ "fun" <+> pretty ret
    <+> parens (sep (punctuate "," $ map pretty params))

accessMType :: MonadError (Doc ann) m => MType -> [Int] -> m MType
accessMType x [] = return x
accessMType (PointerTy x) (_:is) = accessMType x is
accessMType t@(StructTy xs) (i:is) =
  case atMay xs i of
    Just xt -> accessMType xt is
    Nothing -> throwError $ "out of bounds:" <+> (pretty t <> ",") <+> pretty i
  where atMay (y:_) 0  = Just y
        atMay [] _     = Nothing
        atMay (_:ys) n = atMay ys (n - 1)
accessMType t _ = throwError $ pretty t <+> "is not accessable MType"
