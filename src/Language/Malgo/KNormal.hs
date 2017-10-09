{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Language.Malgo.KNormal where

import           Control.Lens
import           Control.Monad.State
import           Data.Either           ()
import           Language.Malgo.HIR
import           Language.Malgo.Syntax

newtype Env = Env { _idCount :: Int }
  deriving (Show, Eq)

makeLenses ''Env

newId :: State Env Id
newId = do
  c <- use idCount
  idCount .= (c + 1)
  return $ Tmp c

insertLet :: EXPR -> (EXPR -> State Env EXPR) -> State Env EXPR
insertLet v@(VAR _, _) k = k v
insertLet (e, t) k = do
  x <- newId
  (e', t') <- k (VAR x, t)
  return (SEQ (LET x t (e, t), UnitTy) (e', t'), t')

transExpr :: EXPR -> State Env EXPR
transExpr (CALL fn args, ty) = bind args [] (\xs -> return (CALL fn xs, ty))
  where bind :: [EXPR] -> [EXPR] -> ([EXPR] -> State Env EXPR) -> State Env EXPR
        bind [] args' k     = k (reverse args')
        bind (x:xs) args' k = do
          x' <- transExpr x
          insertLet x' (\x'' -> bind xs (x'':args') k)

transExpr (BINOP o e1 e2, ty) = do
  e1' <- transExpr e1
  e2' <- transExpr e2
  insertLet e1' (\x -> insertLet e2' (\y -> return (BINOP o x y, ty)))

transExpr (SEQ e1 e2, ty) = do
  e1' <- transExpr e1
  e2' <- transExpr e2
  return (SEQ e1' e2', ty)

transExpr (IF c t f, ty) =
  insertLet c (\c' -> do
                  t' <- transExpr t
                  f' <- transExpr f
                  return (IF c' t' f', ty))
transExpr x = return x

transDecl :: DECL -> State Env DECL
transDecl (DEF n ty val) = fmap (DEF n ty) (transExpr val)
transDecl (DEFUN n retTy args body) = fmap (DEFUN n retTy args) (transExpr body)
transDecl e = return e

trans :: HIR 'Typed -> (HIR 'KNormal, Env)
trans (HIR typed) = runState (transDecl typed) (Env 0) & _1 %~ HIR
