{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.Beta
  ( betaTrans
  , Beta
  ) where

import           Language.Malgo.HIR
import           Language.Malgo.Prelude
import           Language.Malgo.TypeCheck (TypedID (..))
import           Language.Malgo.Utils

newtype BEnv = BEnv
               { _table :: Map TypedID TypedID
               }

instance Env BEnv where
  initEnv = BEnv mempty

type Beta m a = MalgoT BEnv m a

betaTrans :: Monad m => Expr TypedID -> Beta m (Expr TypedID)
betaTrans = transExpr

addBind :: Monad m => TypedID -> TypedID -> Beta m ()
addBind x y = modify $ \e -> e {_table = insert x y (_table e)}

find :: Monad m => TypedID -> Beta m TypedID
find x = do
  table <- gets _table
  let x' = lookup x table
  return $ fromMaybe x x'

transExpr :: Monad m => Expr TypedID -> Beta m (Expr TypedID)
transExpr (Let (FunDec fn params fnbody) body) =
  Let <$> (FunDec fn params <$> transExpr fnbody) <*> transExpr body
transExpr (Let (ValDec name val) body) = do
  val' <- transExpr val
  case val' of
    (Var x) -> addBind name x >> Let (ValDec name val') <$> transExpr body
    _       -> Let (ValDec name val') <$> transExpr body
transExpr (BinOp op x y) = BinOp op <$> find x <*> find y
transExpr (If c t f) = If <$> find c <*> transExpr t <*> transExpr f
transExpr (Call fn args) = Call <$> find fn <*> mapM find args
transExpr (Var x) = Var <$> find x
transExpr x = return x
