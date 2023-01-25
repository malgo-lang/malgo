{-# LANGUAGE TemplateHaskell #-}

module Koriel.Core.LambdaLift
  ( lambdalift,
  )
where

import Control.Lens (At (at), Lens', lens, makeFieldsNoPrefix, traverseOf, traversed, use, (<>=), (?=))
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Koriel.Core.Flat
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq
import Koriel.Prelude
import Relude.Extra.Map (member)

data LambdaLiftState = LambdaLiftState
  { _funcs :: HashMap (Id Type) ([Id Type], Exp (Id Type)),
    _knowns :: HashSet (Id Type)
  }

funcs :: Lens' LambdaLiftState (HashMap (Id Type) ([Id Type], Exp (Id Type)))
funcs = lens (._funcs) (\l x -> l {_funcs = x})

knowns :: Lens' LambdaLiftState (HashSet (Id Type))
knowns = lens (._knowns) (\l x -> l {_knowns = x})

data LambdaLiftEnv = LambdaLiftEnv
  { _uniqSupply :: UniqSupply,
    _moduleName :: ModuleName
  }

makeFieldsNoPrefix ''LambdaLiftEnv

lambdalift :: MonadIO m => UniqSupply -> ModuleName -> Program (Id Type) -> m (Program (Id Type))
lambdalift _uniqSupply _moduleName Program {..} =
  runReaderT ?? LambdaLiftEnv {..} $
    evalStateT ?? LambdaLiftState {_funcs = mempty, _knowns = HashSet.fromList $ map fst topFuncs} $ do
      topFuncs <- traverse (\(f, (ps, e)) -> (f,) . (ps,) <$> llift e) topFuncs
      funcs <>= HashMap.fromList topFuncs
      knowns <>= HashSet.fromList (map fst topFuncs)
      LambdaLiftState {_funcs} <- get
      -- TODO: lambdalift topVars
      traverseOf appProgram (pure . flat) $ Program topVars (HashMap.toList _funcs) extFuncs

llift :: (MonadIO f, MonadState LambdaLiftState f, MonadReader LambdaLiftEnv f) => Exp (Id Type) -> f (Exp (Id Type))
llift (Call (Var f) xs) = do
  ks <- use knowns
  if f `member` ks then pure $ CallDirect f xs else pure $ Call (Var f) xs
llift (Let [LocalDef n (Fun xs call@Call {})] e) = do
  call' <- llift call
  Let [LocalDef n (Fun xs call')] <$> llift e
llift (Let [LocalDef n o@(Fun _ RawCall {})] e) = Let [LocalDef n o] <$> llift e
llift (Let [LocalDef n o@(Fun _ CallDirect {})] e) = Let [LocalDef n o] <$> llift e
llift (Let [LocalDef n (Fun as body)] e) = do
  backup <- get
  ks <- use knowns
  -- nがknownだと仮定してlambda liftする
  knowns . at n ?= ()
  body' <- llift body
  funcs . at n ?= (as, body')
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
  funcs . at f ?= (xs, e)
  pure f
