{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Malgo.Alpha where

import           Control.Monad.State
import           Language.Malgo.HIR    hiding (Env, trans, transDecl, transExpr,
                                        transSeq)
import           Language.Malgo.Syntax (Name, mkName)

type Env = [(Name, Name)]

transDecl :: DECL -> State Env DECL
transDecl = undefined

transExpr :: EXPR -> State Env EXPR
transExpr = undefined

trans :: HIR 'KNormal -> HIR 'Alpha
trans hir = HIR (evalState (transDecl (unHIR hir)) [])
