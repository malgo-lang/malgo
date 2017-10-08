{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Malgo.KNormal where

import           Control.Monad.State
import           Language.Malgo.HIR    hiding (trans, transDecl, transExpr,
                                        transSeq)
import           Language.Malgo.Syntax (Name, mkName)

type Env = [(Name, Name)]

transDecl :: DECL -> State Env DECL
transDecl = undefined

transExpr :: EXPR -> State Env EXPR
transExpr = undefined

trans :: HIR 'Raw -> HIR 'KNormal
trans hir = HIR (evalState (transDecl (unHIR hir)) [])
