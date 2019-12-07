{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
module Language.Malgo.BackEnd.New.LLVM ( GenLLVM ) where

import           Language.Malgo.ID
import           Language.Malgo.IR.LIR
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.TypeRep.Type
import qualified LLVM.AST
import qualified LLVM.AST.Constant           as C
import qualified LLVM.AST.Operand            as O
import qualified LLVM.AST.Type               as LT
import           LLVM.IRBuilder              as IRBuilder
import           Relude                      hiding (Type)

data GenLLVM

instance Pass GenLLVM (Program Type TypedID) [LLVM.AST.Definition] where
  isDump _ = False -- TODO: support dump llvm-ir ast
  trans = undefined
