{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Malgo.KNormal where

import           Control.Monad.State
import           Language.Malgo.HIR    hiding (trans, transDecl, transExpr,
                                        transSeq)
import           Language.Malgo.Syntax (Name, mkName)

type Env = Int

newId :: State Env Id
newId = do
  c <- get
  modify (+1)
  return $ Tmp c

transDecl :: DECL -> State Env DECL
transDecl (DEF i ty val) = do
  val' <- transExpr val
  return $ DEF i ty val'
transDecl (DEFUN i retty params body) = do
  body' <- transExpr body
  return $ DEFUN i retty params body'
transDecl x = return x

transExpr :: EXPR -> State Env EXPR
transExpr v@(VAR _)    = return v
transExpr v@(INT _)    = return v
transExpr v@(FLOAT _)  = return v
transExpr v@(CHAR _)   = return v
transExpr v@(STRING _) = return v
transExpr UNIT         = return UNIT
-- transExpr (CALL i args) = insertLet args (CALL i)

-- transExpr (BLOCK exprs) = do
--   (_, exprs') <- insertLet exprs
--   return $ BLOCK exprs'
-- transExpr (LET i ty val body) = do
--   (ids, exprs) <- insertLet val
--   undefined

insertLet (VAR x) k = k x
insertLet e = do
  x <- newId
  e' <- k x
trans :: HIR 'Raw -> HIR 'KNormal
trans hir = HIR (evalState (transDecl (unHIR hir)) 0)
