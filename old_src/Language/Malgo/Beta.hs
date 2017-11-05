{-# LANGUAGE DataKinds #-}
module Language.Malgo.Beta where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Maybe            (fromMaybe)
import           Language.Malgo.HIR
import           Language.Malgo.Syntax (Name)

type Env = [(Id, Id)]

addBind :: (Id, Id) -> State Env ()
addBind (x, y) = modify ((x, y):)

find :: Id -> State Env Id
find x = do
  env <- get
  let x' = lookup x env
  return $ fromMaybe x x'

transDECL :: DECL 'KNormal -> State Env (DECL 'KNormal)
transDECL (DEF i t v) = do
  v' <- transEXPR v
  return (DEF i t v')
transDECL (DEFUN fn retTy params body) = do
  body' <- transEXPR body
  return (DEFUN fn retTy params body')
transDECL e = return e

transEXPR :: EXPR 'KNormal -> State Env (EXPR 'KNormal)
transEXPR (e, t) = do
  e' <- transEXPR' e
  return (e', t)

transEXPR' :: EXPR' 'KNormal -> State Env (EXPR' 'KNormal)
transEXPR' (BINOP op e1 e2) = BINOP op <$> transEXPR e1 <*> transEXPR e2
transEXPR' (IF c t f) = IF <$> transEXPR c <*> transEXPR t <*> transEXPR f
transEXPR' (LET i t v (e, et)) = do
  v' <- transEXPR v
  case v' of
    (VAR x, _) -> addBind (i, x) >> transEXPR' e
    _          -> LET i t v' <$> transEXPR (e, et)
transEXPR' (VAR x) = VAR <$> find x
transEXPR' (CALL fn args) = CALL <$> find fn <*> mapM transEXPR args
transEXPR' x = return x

trans :: HIR 'KNormal -> (HIR 'KNormal, Env)
trans (HIR d) = runState (transDECL d) [] & _1 %~ HIR
