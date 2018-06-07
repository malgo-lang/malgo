{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
module Language.Malgo.Backend.MIR where

import           Language.Malgo.Prelude
import           Language.Malgo.Type

newtype Program a = Program [Defn a]
  deriving (Show, Eq, Read)

instance Pretty a => Pretty (Program a) where
  pretty (Program defns) =
    vsep (map pretty defns)

data Defn a = DefFun a [a] (Expr a)
            | DefEx a Text
  deriving (Show, Eq, Read)

instance Pretty a => Pretty (Defn a) where
  pretty (DefFun fn params body) =
    "define" <+> pretty fn <> parens (sep (punctuate "," $ map pretty params)) <> softline <> braces (indent 2 (pretty body))
  pretty (DefEx fn name) =
    "declare" <+> pretty fn <+> "=" <+> pretty name

data Expr a = Var a
            | Int Integer
            | Float Double
            | Char Char
            | String Text
            | Unit
            | Tuple [a]
            | Apply a [a]
            | Let a (Expr a) (Expr a)
            | Cast MType a
            | Access (Expr a) [Int]
            | If a (Expr a) (Expr a)
  deriving (Show, Eq, Read)

instance Pretty a => Pretty (Expr a) where
  pretty (Var a) = pretty a
  pretty (Int i) = pretty i
  pretty (Float d) = pretty d
  pretty (Char c) = squotes $ pretty c
  pretty (String s) = dquotes $ pretty s
  pretty Unit = lparen <> rparen
  pretty (Tuple xs) = parens $ align $ sep $ punctuate "," $ map pretty xs
  pretty (Apply f args) = pretty f <> parens (align $ sep (punctuate "," $ map pretty args))
  pretty (Let name val body) =
    pretty name <+> "=" <+> pretty val
    <> line <> pretty body
  pretty (Cast ty val) = "cast" <+> pretty ty <+> pretty val
  pretty (Access e is) = "access" <+> pretty e <+> brackets (align $ sep (punctuate "," $ map pretty is))
  pretty (If c t f) =
    "if" <+> parens (pretty c)
    <+> braces (pretty t)
    <+> "else" <+> braces (pretty f)

data MType = IntTy Integer
           | DoubleTy
           | PointerTy MType
           | StructTy [MType]
           | FunctionTy MType [MType]
  deriving (Show, Eq, Read)

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

toMType :: (Typeable a, Pretty a, MonadError (Doc ann) m) => a -> m MType
toMType (typeOf -> NameTy n) =
  case n of
    "Int"    -> return $ IntTy 32
    "Float"  -> return DoubleTy
    "Bool"   -> return $ IntTy 1
    "Char"   -> return $ IntTy 8
    "String" -> return $ PointerTy (IntTy 8)
    "Unit"   -> return $ StructTy []
    _        -> throwError $ pretty n <+> "is not valid type"
toMType (typeOf -> FunTy params ret) =
  FunctionTy <$> toMType ret <*> mapM toMType params
toMType (typeOf -> TupleTy xs) =
  PointerTy . StructTy <$> mapM toMType xs
toMType (typeOf -> ClsTy{}) =
  throwError "ClsTy does not have MType"
toMType x = throwError $ "unreachable:" <+> pretty x
