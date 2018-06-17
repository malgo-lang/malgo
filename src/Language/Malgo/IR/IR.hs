{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE ViewPatterns          #-}
module Language.Malgo.IR.IR where

import           Data.List               (delete, (\\))
import           Language.Malgo.FreeVars
import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Type

newtype Program a = Program [Defn a]
  deriving (Show, Eq, Read)

instance FreeVars Program where
  freevars (Program xs) = concatMap freevars xs

instance Pretty a => Pretty (Program a) where
  pretty (Program defns) =
    vsep (map pretty defns)

data Defn a = DefFun a [a] (Expr a)
            | DefEx a Text
  deriving (Show, Eq, Read)

exToFun :: MonadMalgo s m => Defn (ID MType) -> m (Defn (ID MType))
exToFun d@(DefEx name orig) = do
  let ty = mTypeOf name
  case ty of
    FunctionTy ret params -> do
      params' <- mapM (newID "x") params
      prim <- newID orig ty
      return $ DefFun name params' (Let prim (PrimFun orig ret params) (Apply prim params'))
    _ -> return d
exToFun d = return d

instance FreeVars Defn where
  freevars (DefFun _ params body) =
    freevars body \\ params
  freevars (DefEx _ _) = []

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
            | PrimFun Text MType [MType]
            | Tuple [a]
            | Apply a [a]
            | Let a (Expr a) (Expr a)
            | LetRec [(a, [a], Expr a)] (Expr a)
            | Cast MType a
            | Access a [Int]
            | If a (Expr a) (Expr a)
  deriving (Show, Eq, Read)

primFuns :: Expr a -> [Expr a]
primFuns p@PrimFun{} = [p]
primFuns (Let _ v e) = primFuns v ++ primFuns e
primFuns (LetRec vs e) = primFuns' ++ primFuns e
  where primFuns' = concatMap (primFuns . view _3) vs
primFuns (If _ t f) = primFuns t ++ primFuns f
primFuns _ = []

instance FreeVars Expr where
  freevars (Var x) = [x]
  freevars (Tuple xs) = xs
  freevars (Apply _ args) = args
  freevars (Let x v e) = freevars v ++ delete x (freevars e)
  freevars (LetRec xs e) =
    (concatMap (\(_, params, body) -> freevars body \\ params) xs ++ freevars e)
    \\ map (view _1) xs
  freevars (Cast _ x) = [x]
  freevars (Access x _) = [x]
  freevars (If c t f) = c : freevars t ++ freevars f
  freevars _ = []

instance Pretty a => Pretty (Expr a) where
  pretty (Var a) = pretty a
  pretty (Int i) = pretty i
  pretty (Float d) = pretty d
  pretty (Char c) = squotes $ pretty c
  pretty (String s) = dquotes $ pretty s
  pretty Unit = lparen <> rparen
  pretty (PrimFun name ret params) = "#" <> pretty name <> braces (pretty $ FunctionTy ret params)
  pretty (Tuple xs) = parens $ align $ sep $ punctuate "," $ map pretty xs
  pretty (Apply f args) = pretty f <> parens (align $ sep (punctuate "," $ map pretty args))
  pretty (Let name val body) =
    pretty name <+> "=" <+> pretty val
    <> line <> pretty body
  pretty (LetRec defs body) =
    align (vsep (map (\(name, params, val) -> "rec" <+> pretty name <+> sep (map pretty params) <+> "=" <+> pretty val) defs))
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
  mTypeOf (Char _) = IntTy 8
  mTypeOf (String _) = PointerTy (IntTy 8)
  mTypeOf Unit = StructTy []
  mTypeOf (PrimFun _ ret params) = FunctionTy ret params
  mTypeOf (Tuple xs) = PointerTy (StructTy (map mTypeOf xs))
  mTypeOf (Apply f _) =
    case mTypeOf f of
      FunctionTy t _ -> t -- normal function
      PointerTy (StructTy [FunctionTy t _, PointerTy (StructTy _)] ) -> t -- closure
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
