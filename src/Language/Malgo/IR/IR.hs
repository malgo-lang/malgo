{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module Language.Malgo.IR.IR where

import           Data.List               (delete, (\\))
import           Language.Malgo.FreeVars
import           Language.Malgo.ID
import           Language.Malgo.Prelude

newtype Program a = Program [Defn a]
  deriving (Show, Eq, Read)

instance FreeVars Program where
  freevars (Program xs) = concatMap freevars xs

instance Pretty a => Pretty (Program a) where
  pretty (Program defns) =
    vsep (map pretty defns)

data Defn a = DefFun a [a] (Expr a)
  deriving (Show, Eq, Read)

instance FreeVars Defn where
  freevars (DefFun _ params body) =
    freevars body \\ params

instance Pretty a => Pretty (Defn a) where
  pretty (DefFun fn params body) =
    "define" <+> pretty fn <> parens (sep (punctuate "," $ map pretty params))
    <> softline <> braces (indent 2 (pretty body))

{- Closure representation

Tuple [fn :: FunctionTy ret [PointerTy (IntTy 8), x, y, ...], Cast (PointerTy (IntTy 8)) (Tuple ..)]

Apply (Tuple [fn, env]) args -> Apply fn (env : args)

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
  deriving (Show, Eq, Read)

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
  freevars (Let x v e) = freevars v ++ delete x (freevars e)
  freevars (LetRec xs e) =
    (concatMap (\(_, params, body) -> freevars body \\ fromMaybe [] params) xs ++ freevars e)
    \\ map (view _1) xs
  freevars (Cast _ x) = [x]
  freevars (Access x _) = [x]
  freevars (If c t f) = c : freevars t ++ freevars f
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
  pretty (Prim name ty) = "#" <> pretty name <> braces (pretty ty)
  pretty (Tuple xs) = parens $ align $ sep $ punctuate "," $ map pretty xs
  pretty (Apply f args) = pretty f <> parens (align $ sep (punctuate "," $ map pretty args))
  pretty (Let name val body) =
    pretty name <+> "=" <+> pretty val
    <> line <> pretty body
  pretty (LetRec defs body) =
    align (vsep (map (\(name, params, val) -> "rec" <+> pretty name
                       <+> sep (map pretty (fromMaybe [] params)) <+> "=" <+> pretty val) defs))
    <> line <> pretty body
  pretty (Cast ty val) = "cast" <+> pretty ty <+> pretty val
  pretty (Access e is) = "access" <+> pretty e <+> brackets (align $ sep (punctuate "," $ map pretty is))
  pretty (If c t f) =
    "if" <+> parens (pretty c)
    <+> braces (pretty t)
    <+> "else" <+> braces (pretty f)


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
      PointerTy (StructTy [FunctionTy t _, _]) -> t -- closure
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
  deriving (Show, Eq, Read)

class HasMType a where
  mTypeOf :: a -> MType

instance HasMType MType where
  mTypeOf = identity

instance HasMType a => HasMType (ID a) where
  mTypeOf = mTypeOf . _idMeta

instance Pretty MType where
  pretty (IntTy i) = "i" <> pretty i
  pretty DoubleTy = "double"
  pretty (PointerTy t) = parens $ "ptr" <+> pretty t
  pretty (StructTy ts) = parens $ "struct" <+> align (sep (punctuate "," $ map pretty ts))
  pretty (FunctionTy ret params) =
    parens $ "fun" <+> pretty ret
    <+> parens (align $ sep (punctuate "," $ map pretty params))

accessMType :: MonadError (Doc ann) m => MType -> [Int] -> m MType
accessMType x [] = return x
accessMType (PointerTy x) (_:is) = accessMType x is
accessMType t@(StructTy xs) (i:is) =
  case atMay xs i of
    Just xt -> accessMType xt is
    Nothing -> throwError $ "out of bounds:" <+> pretty t <> "," <+> pretty i
accessMType t _ = throwError $ pretty t <+> "is not accessable MType"
