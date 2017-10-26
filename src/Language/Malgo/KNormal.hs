{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Malgo.KNormal where

import           Control.Monad.State
import           Data.String
import           Language.Malgo.Types
import qualified Language.Malgo.Typing as T

data Decl = DefVar Id Type Const
          | DefFun Id Type [(Id, Type)] Expr
          | ExVar Id Type
          | ExFun Id Type [(Id, Type)]
  deriving (Eq, Show)

data Const = Int Integer
           | Float Double
           | Bool Bool
           | Char Char
           | String String
           | Unit
           | CBinOp Op Const Const
  deriving (Eq, Show)

type Expr = (Expr', Type)

-- 第一引数のTypeが式の型を表す
data Expr' = Var Id
           | Const Const
           | Call Id [Id] -- 第二引数が引数の型を表す
           | Let Id Type Expr Expr
           | If Id Expr Expr
           | BinOp Op Id Id -- 第二引数が引数の型を表す
  deriving (Eq, Show)

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
                             , (Id (c, Name $ fromName hint ++ show c))
                             ) : (table e)
                   }
  return (Id (c, Name $ fromName hint ++ show c))

getId :: Name -> KNormal Id
getId name = do
  t <- gets table
  case lookup name t of
    Just x  -> return x
    Nothing -> newId name

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
  undefined -- letに変換

transConst :: T.Const -> KNormal Const
transConst (T.Int x)         = return (Int x)
transConst (T.Float x)       = return (Float x)
transConst (T.Bool x)        = return (Bool x)
transConst (T.Char x)        = return (Char x)
transConst (T.String x)      = return (String x)
transConst T.Unit            = return Unit
transConst (T.CBinOp op x y) = CBinOp op <$> transConst x <*> transConst y
