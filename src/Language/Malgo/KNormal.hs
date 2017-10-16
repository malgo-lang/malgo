{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Language.Malgo.KNormal where

import           Control.Lens
import           Control.Monad.State
import           Data.Either         ()
import           Language.Malgo.HIR

newtype Env = Env { _idCount :: Int }
  deriving (Show, Eq)

makeLenses ''Env

newId :: State Env Id
newId = do
  c <- use idCount
  idCount .= (c + 1)
  return $ Sym ("$k" ++ show c)

insertLet :: EXPR 'Typed -> (EXPR 'KNormal -> State Env (EXPR 'KNormal)) -> State Env (EXPR 'KNormal)
-- insertLet v@(VAR _, _) k = k v
insertLet v@(_, t) k = do
  x <- newId
  v' <- transExpr v
  (e', t') <- k (VAR x, t)
  return (LET x t v' (e', t'), t')

transExpr :: EXPR 'Typed -> State Env (EXPR 'KNormal)
transExpr (CALL fn args, ty) = bind args [] (\xs -> return (CALL fn xs, ty))
  where bind :: [EXPR 'Typed] -> [EXPR 'KNormal] -> ([EXPR 'KNormal] -> State Env (EXPR 'KNormal)) -> State Env (EXPR 'KNormal)
        bind [] args' k     = k (reverse args')
        bind (x:xs) args' k = insertLet x (\x' -> bind xs (x':args') k)

transExpr (BINOP o e1 e2, ty) = insertLet e1 (\x -> insertLet e2 (\y -> return (BINOP o x y, ty)))

transExpr (IF c t f, ty) =
  insertLet c (\c' -> do
                  t' <- transExpr t
                  f' <- transExpr f
                  return (IF c' t' f', ty))

transExpr (VAR x, t) = return (VAR x, t)
transExpr (INT x, t) = return (INT x, t)
transExpr (FLOAT x, t) = return (FLOAT x, t)
transExpr (BOOL x, t) = return (BOOL x, t)
transExpr (CHAR x, t) = return (CHAR x, t)
transExpr (STRING x, t) = return (STRING x, t)
transExpr (UNIT, t) = return (UNIT, t)
transExpr (LET x t e b, ty) = do
  e' <- transExpr e
  b' <- transExpr b
  return (LET x t e' b', ty)

transDecl :: DECL 'Typed -> State Env (DECL 'KNormal)
transDecl (DEF n ty val) = fmap (DEF n ty) (transExpr val)
transDecl (DEFUN n retTy args body) = fmap (DEFUN n retTy args) (transExpr body)
transDecl (EXDEF n ty) = return $ EXDEF n ty
transDecl (EXDEFUN n retTy args) = return $ EXDEFUN n retTy args

trans :: HIR 'Typed -> (HIR 'KNormal, Env)
trans (HIR typed) = runState (transDecl typed) (Env 0) & _1 %~ HIR
