{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Language.Malgo.KNormal where

import           Control.Monad.State
import           Data.String
import qualified Language.Malgo.Typing as T
import           Language.Malgo.Utils
import           Text.PrettyPrint

type Expr = (Expr', Type)

instance PrettyPrint Expr where
  -- pretty (e, t) = pretty e <> colon <> pretty t
  pretty (e, _) = pretty e

data Expr' =
  -- | 変数参照
    Var Name
  -- | 32bit整数
  | Int Integer
  -- | 倍精度浮動小数点数
  | Float Double
  -- | 真(#t) 偽(#f)
  | Bool Bool
  -- | シングルクォートで囲まれた一文字
  | Char Char
  -- | ダブルクォートで囲まれた文字列
  | String String
  -- | 空の値("()")
  | Unit
  -- | 関数呼び出し
  | Call (Name, Type) [(Name, Type)]
  -- | let式
  | Let Decl Expr
  -- | if式
  | If (Name, Type) Expr Expr
  -- | 中置演算子
  | BinOp Op (Name, Type) (Name, Type)
  deriving (Eq, Show)

instance PrettyPrint (Name, Type) where
  -- pretty (i, t) = pretty i <> colon <> pretty t
  pretty (i, _) = pretty i

instance PrettyPrint Expr' where
  pretty (Var name)     = pretty name
  pretty (Int x)         = integer x
  pretty (Float x)       = double x
  pretty (Bool True)     = text "#t"
  pretty (Bool False)    = text "#f"
  pretty (Char x)        = quotes $ char x
  pretty (String x)      = doubleQuotes $ text x
  pretty Unit          = text "()"
  pretty (Call (fn, ty) arg) = parens $ sep (pretty fn {- <> colon <> pretty ty -} : map pretty arg)
  pretty (Let decl body) =
    parens $ text "let" <+> pretty decl
    $+$ nest 2 (pretty body)
  pretty (If c t f) =
    parens $ text "if" <+> pretty c
    $+$ nest 2 (pretty t)
    $+$ nest 2 (pretty f)
  pretty (BinOp op x y) = parens $ sep [pretty op, pretty x, pretty y]

-- | Malgoの組み込みデータ型
data Type = NameTy Name
          | TupleTy [Type]
          | FunTy Type Type
          | ClsTy Type [Type]
  deriving (Eq, Show)

instance PrettyPrint Type where
  pretty (NameTy n)          = pretty n
  pretty (TupleTy types)     = parens (cat $ punctuate (text ",") $ map pretty types)
  pretty (FunTy domTy codTy) = pretty domTy <+> text "->" <+> pretty codTy
  pretty (ClsTy ty fv) = braces (pretty ty <+> brackets (sep (map pretty fv)))

data Decl = FunDec Name [(Name, Type)] Type Expr
          | ValDec Name Type Expr
  deriving (Eq, Show)

instance PrettyPrint Decl where
  pretty (FunDec name params retTy body) = parens $
    text "fun" <+> (parens . sep $ pretty name <> colon <> pretty retTy : map (\(n, t) -> pretty n <> colon <> pretty t) params)
    $+$ nest 2 (pretty body)
  pretty (ValDec name typ val) = parens $
    text "val" <+> pretty name <> colon <> pretty typ <+> pretty val

data KNormalState = KNormalState { count :: Int
                                 , table :: [(Name, Name)]
                                 }
  deriving Show

newtype KNormal a = KNormal (StateT KNormalState (Either String) a)
  deriving (Functor, Applicative, Monad, MonadState KNormalState)

runKNormal :: KNormal a -> Either String (a, KNormalState)
runKNormal (KNormal m) = runStateT m (KNormalState 0 [])

knormal :: T.Expr -> Either String (Expr, Int)
knormal a = do
  k <- runKNormal $ transExpr a
  return (fst k, count (snd k))

newId :: Name -> KNormal Name
newId hint = do
  c <- gets count
  modify $ \e -> e { count = count e + 1
                   , table = ( hint
                             , hint `mappend` "." `mappend` fromString (show c)
                             ) : table e
                   }
  return (hint `mappend` "." `mappend` fromString (show c))

incCount :: Int -> KNormal ()
incCount n = modify $ \e -> e { count = count e + n }

getId :: Name -> KNormal Name
getId name = do
  t <- gets table
  case lookup name t of
    Just x  -> return x
    Nothing -> rawId name

rawId :: Name -> KNormal Name
rawId name = do
  t <- gets table
  case lookup name t of
    Just x -> return x
    Nothing -> do
      modify $ \e -> e { table = (name, name) : table e }
      return name

insertLet :: T.Expr -> ((Name, Type) -> KNormal Expr) -> KNormal Expr
insertLet v@(_, t) k = do
  x <- newId "$k"
  v' <- transExpr v
  t' <- transType t
  e' <- k (x, t')
  return (Let (ValDec x t' v') e', snd e')

transType :: T.Type -> KNormal Type
transType (T.NameTy name) = NameTy <$> rawId name
transType (T.TupleTy tys) = TupleTy <$> mapM transType tys
transType (T.FunTy domTy codTy) = FunTy <$> transType domTy <*> transType codTy

transExpr :: T.Expr -> KNormal Expr
transExpr (T.Call (T.Var fn, funTy) args, ty) = do
  fn' <- getId fn
  funTy' <- transType funTy
  ty' <- transType ty
  bind args [] (\xs -> return (Call (fn', funTy') xs, ty'))
  where
    bind [] args' k     = k (reverse args')
    bind (x:xs) args' k = insertLet x (\x' -> bind xs (x':args') k)
transExpr (T.Call _ _, _) = KNormal $ lift . Left $ "error: function value must be a variable"
transExpr (T.BinOp op e1 e2, ty) = do
  ty' <- transType ty
  insertLet e1 (\x -> insertLet e2 (\y -> return (BinOp op x y, ty')))
transExpr (T.If c t f, ty) =
  insertLet c (\c' -> do
                  t' <- transExpr t
                  f' <- transExpr f
                  ty' <- transType ty
                  return (If c' t' f', ty'))
transExpr (T.Int x, ty) = (,) <$> pure (Int x) <*> transType ty
transExpr (T.Float x, ty) = (,) <$> pure (Float x) <*> transType ty
transExpr (T.Bool x, ty) = (,) <$> pure (Bool x) <*> transType ty
transExpr (T.Char x, ty) = (,) <$> pure (Char x) <*> transType ty
transExpr (T.String x, ty) = (,) <$> pure (String x) <*> transType ty
transExpr (T.Unit, ty) = (,) <$> pure Unit <*> transType ty
transExpr (T.Let (T.ValDec name typ val) body, ty) = do
  val' <- transExpr val
  typ' <- transType typ

  name' <- newId name -- shadowingのため、先にvalを処理する

  body' <- transExpr body
  ty' <- transType ty
  return (Let (ValDec name' typ' val') body', ty')
transExpr (T.Let (T.FunDec fn params retTy fbody) body, ty) = do
  fn' <- newId fn
  params' <- mapM (\(n, t) -> (,) <$> newId n <*> transType t) params
  retTy' <- transType retTy
  fbody' <- transExpr fbody
  body' <- transExpr body
  ty' <- transType ty
  return (Let (FunDec fn' params' retTy' fbody') body', ty')
transExpr (T.Var x, ty) = (,) <$> (Var <$> getId x) <*> transType ty
transExpr (T.Seq e1 e2, ty) = do
  x' <- newId "_"
  e1' <- transExpr e1
  e2' <- transExpr e2
  unitTy <- NameTy <$> rawId "Unit"
  ty' <- transType ty
  return (Let (ValDec x' unitTy e1') e2', ty')
