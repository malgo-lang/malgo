{-# LANGUAGE TemplateHaskell #-}

module Koriel.Core.LambdaLift
  ( lambdalift,
  )
where

import Control.Lens (makeFieldsNoPrefix, traverseOf, traversed)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Koriel.Core.Flat
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Relude.Extra.Map (member)

data LambdaLiftState = LambdaLiftState
  { funcs :: HashMap (Id Type) ([Id Type], Exp (Id Type)),
    knowns :: HashSet (Id Type)
  }

updateFuncs :: [(Id Type, ([Id Type], Exp (Id Type)))] -> LambdaLiftState -> LambdaLiftState
updateFuncs kvs s = s {funcs = HashMap.fromList kvs <> s.funcs}

updateKnowns :: [Id Type] -> LambdaLiftState -> LambdaLiftState
updateKnowns kvs s = s {knowns = HashSet.fromList kvs <> s.knowns}

data LambdaLiftEnv = LambdaLiftEnv
  { uniqSupply :: UniqSupply,
    _moduleName :: ModuleName
  }

makeFieldsNoPrefix ''LambdaLiftEnv

lambdalift :: MonadIO m => UniqSupply -> ModuleName -> Program (Id Type) -> m (Program (Id Type))
lambdalift uniqSupply _moduleName Program {..} =
  runReaderT ?? LambdaLiftEnv {..} $
    evalStateT ?? LambdaLiftState {funcs = mempty, knowns = HashSet.fromList $ map fst topFuncs} $ do
      topFuncs <- traverse (\(f, (ps, e)) -> (f,) . (ps,) <$> llift e) topFuncs
      modify (updateFuncs topFuncs)
      modify (updateKnowns $ map fst topFuncs)
      LambdaLiftState {funcs} <- get
      -- TODO: lambdalift topVars
      traverseOf appProgram (pure . flat) $ Program topVars (HashMap.toList funcs) extFuncs

llift :: (MonadIO f, MonadState LambdaLiftState f, MonadReader LambdaLiftEnv f) => Exp (Id Type) -> f (Exp (Id Type))
llift (Call (Var f) xs) = do
  ks <- gets (.knowns)
  if f `member` ks then pure $ CallDirect f xs else pure $ Call (Var f) xs
llift (Let [LocalDef n (Fun xs call@Call {})] e) = do
  call' <- llift call
  Let [LocalDef n (Fun xs call')] <$> llift e
llift (Let [LocalDef n o@(Fun _ RawCall {})] e) = Let [LocalDef n o] <$> llift e
llift (Let [LocalDef n o@(Fun _ CallDirect {})] e) = Let [LocalDef n o] <$> llift e
llift (Let [LocalDef n (Fun as body)] e) = do
  backup <- get
  ks <- gets (.knowns)
  -- nがknownだと仮定してlambda liftする
  modify (updateKnowns [n])
  body' <- llift body
  modify (updateFuncs [(n, (as, body'))])
  (e', _) <- localState $ llift e
  -- (Fun as body')の自由変数がknownsを除いてなく、e'の自由変数にnが含まれないならnはknown
  -- (Call n _)は(CallDirect n _)に変換されているので、nが値として使われているときのみ自由変数になる
  let fvs = HashSet.difference (freevars body') (ks <> HashSet.fromList as)
  if null fvs && not (n `member` freevars e')
    then llift e
    else do
      put backup
      body' <- llift body
      let fvs = HashSet.difference (freevars body') (ks <> HashSet.fromList as)
      newFun <- def n.name (toList fvs <> as) body'
      Let [LocalDef n (Fun as (CallDirect newFun $ map Var $ toList fvs <> as))] <$> llift e
llift (Let ds e) = Let ds <$> llift e
llift (Match e cs) = Match <$> llift e <*> traverseOf (traversed . appCase) llift cs
llift e = pure e

def :: (MonadIO m, MonadState LambdaLiftState m, MonadReader LambdaLiftEnv m) => Text -> [Id Type] -> Exp (Id Type) -> m (Id Type)
def name xs e = do
  f <- newTemporalId ("raw_" <> name) (map typeOf xs :-> typeOf e)
  modify $ updateFuncs [(f, (xs, e))]
  pure f
