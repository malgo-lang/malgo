{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Malgo.Beta where

import           Control.Monad.State
import           Data.Maybe             (fromMaybe)
import qualified Language.Malgo.KNormal as K
import           Language.Malgo.Types

newtype BetaTransState = BetaTransState { table :: [(Id, Id)]
                                        }
  deriving Show

newtype BetaTrans a = BetaTrans (StateT BetaTransState (Either String) a)
  deriving (Functor, Applicative, Monad, MonadState BetaTransState)

betaTrans :: BetaTrans a -> Either String a
betaTrans (BetaTrans m) = evalStateT m (BetaTransState [])

addBind :: (Id, Id) -> BetaTrans ()
addBind (x, y) =
  modify $ \e -> e { table = (x, y) : table e }

find :: Id -> BetaTrans Id
find x = do
  env <- gets table
  let x' = lookup x env
  return $ fromMaybe x x'

transDecl :: K.Decl -> BetaTrans K.Decl
transDecl (K.DefFun fn retTy params body) =
  K.DefFun fn retTy params <$> transExpr body
transDecl x = return x

transExpr :: K.Expr -> BetaTrans K.Expr
transExpr (K.BinOp op x y, ty) = do
  e <- K.BinOp op <$> find x <*> find y
  return (e, ty)
transExpr (K.If c t f, ty)     = do
  e <- K.If <$> find c <*> transExpr t <*> transExpr f
  return (e, ty)
transExpr (K.Let name typ val body, ty) = do
  val' <- transExpr val
  case val' of
    (K.Var x, _) -> addBind (name, x) >> transExpr body
    _ -> do
      e <- K.Let name typ val' <$> transExpr body
      return (e, ty)
transExpr (K.Call fn args, ty) = do
  e <- K.Call <$> find fn <*> mapM find args
  return (e, ty)
transExpr (K.Var x, ty) = do
  e <- K.Var <$> find x
  return (e, ty)
transExpr (K.Const c, ty) = return (K.Const c, ty)
