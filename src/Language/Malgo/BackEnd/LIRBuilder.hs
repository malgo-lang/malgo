{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.BackEnd.LIRBuilder where

import           Control.Lens                 (snoc)
import           Language.Malgo.ID
import           Language.Malgo.IR.LIR        as L
import           Language.Malgo.IR.MIR        as M
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.LType as L
import           Language.Malgo.TypeRep.Type  as M
import           Relude                       hiding (Type)
import           Relude.Extra.Map             hiding (size)

data ProgramEnv = ProgramEnv { defs        :: [L.Func (ID LType)]
                             , functionMap :: Map (ID Type) (ID LType)}
data ExprEnv = ExprEnv { partialBlockInsts :: [(ID LType, Inst (ID LType))]
                       , variableMap       :: Map (ID Type) (ID LType)
                       , nameHint          :: Text }

runGenProgram mainFunc m = do
  ProgramEnv { defs } <- execStateT m (ProgramEnv [] mempty)
  pure $ L.Program { L.functions = defs, mainFunc = mainFunc }

runGenExpr variableMap nameHint m = do
  ExprEnv { partialBlockInsts } <- execStateT m (ExprEnv [] variableMap nameHint)
  pure $ Block { insts = reverse partialBlockInsts }

addInst :: (MonadState ExprEnv m, MonadMalgo m) => Inst (ID LType) -> m ()
addInst inst = do
  hint <- gets nameHint
  i <- newID (ltypeOf inst) hint
  modify (\s -> s { partialBlockInsts = snoc (partialBlockInsts s) (i, inst)})
