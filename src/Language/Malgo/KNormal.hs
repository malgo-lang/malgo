{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Language.Malgo.KNormal where

import           Control.Monad.State
import           Data.String
import           Language.Malgo.PrettyPrint
import           Language.Malgo.Types
import qualified Language.Malgo.Typing      as T
import           Text.PrettyPrint

data Decl = DefVar Id Type Const
          | DefFun Id Type [(Id, Type)] Expr
          | ExVar Id Type
          | ExFun Id Type [(Id, Type)]
  deriving (Eq, Show)

instance PrettyPrint Decl where
  pretty (DefVar name typ val) =
    parens $ text "def" <+> pretty name <> colon <> pretty typ
    <+> pretty val
  pretty (DefFun fn retTy params body) =
    parens $ text "def" <+> parens (sep (pretty fn <> colon <> pretty retTy : map (\(n, t) -> pretty n <> colon <> pretty t) params))
    $+$ nest 4 (pretty body)
  pretty (ExVar name typ) =
    parens $ text "extern" <+> pretty name <> colon <> pretty typ
  pretty (ExFun fn retTy params) =
    parens $ text "extern" <+> parens (sep (pretty fn <> colon <> pretty retTy : map (\(n, t) -> pretty n <> colon <> pretty t) params))

data Const = Int Integer
           | Float Double
           | Bool Bool
           | Char Char
           | String String
           | Unit
           | CBinOp Op Const Const
  deriving (Eq, Show)

instance PrettyPrint Const where
  pretty (Int x)         = integer x
  pretty (Float x)       = double x
  pretty (Bool True)     = text "#t"
  pretty (Bool False)    = text "#f"
  pretty (Char x)        = quotes $ char x
  pretty (String x)      = doubleQuotes $ text x
  pretty Unit            = text "()"
  pretty (CBinOp op x y) = parens (pretty op <+> pretty x <+> pretty y)

type Expr = (Expr', Type)

instance PrettyPrint Expr where
  pretty (e, t) = pretty e -- <> colon <> pretty t

-- 第一引数のTypeが式の型を表す
data Expr' = Var Id
           | Const Const
           | Call Id [Id] -- 第二引数が引数の型を表す
           | Let Id Type Expr Expr
           | If Id Expr Expr
           | BinOp Op Id Id -- 第二引数が引数の型を表す
  deriving (Eq, Show)

instance PrettyPrint Expr' where
  pretty (Var name)     = pretty name
  pretty (Const c)        = pretty c
  pretty (Call fn args) = parens . sep $ pretty fn : map pretty args
  pretty (Let name typ val body) =
    parens $ text "let" <+> parens (pretty name <> colon <> pretty typ <+> pretty val)
    $+$ nest 2 (pretty body)
  pretty (If c t f) =
    parens $ text "if" <+> pretty c
    $+$ nest 2 (pretty t)
    $+$ nest 2 (pretty f)
  pretty (BinOp op x y) = parens (pretty op <+> pretty x <+> pretty y)

data KNormalState = KNormalState { count :: Int
                                 , table :: [(Name, Id)]
                                 }
  deriving Show

newtype KNormal a = KNormal (StateT KNormalState (Either String) a)
  deriving (Functor, Applicative, Monad, MonadState KNormalState)

knormal :: KNormal a -> Either String a
knormal (KNormal m) = evalStateT m (KNormalState 0 [])

newId :: Name -> KNormal Id
newId hint = do
  c <- gets count
  modify $ \e -> e { count = count e + 1
                   , table = ( hint
                             , Id (c, hint)
                             ) : table e
                   }
  return (Id (c, hint))

getId :: Name -> KNormal Id
getId name = do
  t <- gets table
  case lookup name t of
    Just x  -> return x
    Nothing -> newId name

rawId :: Name -> KNormal Id
rawId name = do
  modify $ \e -> e { table = (name, Raw name) : table e }
  return (Raw name)

insertLet :: T.Expr -> (Id -> KNormal Expr) -> KNormal Expr
insertLet v@(_, t) k = do
  x <- newId (Name "$k")
  v' <- transExpr v
  (e', t') <- k x
  return (Let x t v' (e', t'), t')

transExpr :: T.Expr -> KNormal Expr
transExpr (T.Call fn args, ty) = do
  fn' <- getId fn
  bind args [] (\xs -> return (Call fn' xs, ty))
  where
    bind [] args' k     = k (reverse args')
    bind (x:xs) args' k = insertLet x (\x' -> bind xs (x':args') k)
transExpr (T.BinOp op e1 e2, ty) =
  insertLet e1 (\x -> insertLet e2 (\y -> return (BinOp op x y, ty)))
transExpr (T.If c t f, ty) =
  insertLet c (\c' -> do
                  t' <- transExpr t
                  f' <- transExpr f
                  return (If c' t' f', ty))
transExpr (T.Const val, ty) = do
  val' <- transConst val
  return (Const val', ty)
transExpr (T.Let name typ val body, ty) = do
  name' <- newId name
  val' <- transExpr val
  body' <- transExpr body
  return (Let name' typ val' body', ty)
transExpr (T.Var x, ty) = do
  x' <- getId x
  return (Var x', ty)
transExpr (T.Seq e1 e2, ty) = do
  x' <- newId (Name "_")
  e1' <- transExpr e1
  e2' <- transExpr e2
  return (Let x' UnitTy e1' e2', ty)

transConst :: T.Const -> KNormal Const
transConst (T.Int x)         = return (Int x)
transConst (T.Float x)       = return (Float x)
transConst (T.Bool x)        = return (Bool x)
transConst (T.Char x)        = return (Char x)
transConst (T.String x)      = return (String x)
transConst T.Unit            = return Unit
transConst (T.CBinOp op x y) = CBinOp op <$> transConst x <*> transConst y

transDecl :: T.Decl -> KNormal Decl
transDecl (T.DefVar name typ val) = do
  name' <- rawId name
  val' <- transConst val
  return (DefVar name' typ val')
transDecl (T.DefFun fn retTy params body) = do
  fn' <- rawId fn
  params' <- mapM
    (\(n, ty) -> do
        n' <- newId n
        return (n', ty))
    params
  body' <- transExpr body
  return (DefFun fn' retTy params' body')
transDecl (T.ExVar name typ) = flip ExVar typ <$> rawId name
transDecl (T.ExFun fn retTy params) =
  ExFun <$> rawId fn <*> return retTy
  <*> mapM (\(n, ty) -> do
               n' <- newId n
               return (n', ty))
           params
