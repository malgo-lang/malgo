{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}

module Koriel.Core.LambdaLift
  ( lambdalift,
  )
where

import Control.Lens (At (at), Lens', lens, traverseOf, traversed, use, view, (<>=), (?=), _1, _2)
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
  { _funcs :: HashMap (Id Type) ([Id Type], Type, Exp (Id Type)),
    _knowns :: HashSet (Id Type)
  }

funcs :: Lens' LambdaLiftState (HashMap (Id Type) ([Id Type], Type, Exp (Id Type)))
funcs = lens (._funcs) (\l x -> l {_funcs = x})

knowns :: Lens' LambdaLiftState (HashSet (Id Type))
knowns = lens (._knowns) (\l x -> l {_knowns = x})

data LambdaLiftEnv = LambdaLiftEnv
  { uniqSupply :: UniqSupply,
    moduleName :: ModuleName
  }

data DefEnv = DefEnv {uniqSupply :: UniqSupply, moduleName :: ModuleName}

def :: (MonadIO m, MonadState LambdaLiftState m, MonadReader LambdaLiftEnv m) => Id Type -> [Id Type] -> Exp (Id Type) -> m (Id Type)
def name xs e = do
  uniqSupply <- asks (.uniqSupply)
  f <- runReaderT (newTemporalId ("raw_" <> name.name) (map typeOf xs :-> typeOf e)) (DefEnv uniqSupply name.moduleName)
  -- knowns . at f ?= ()
  modify $ \state@LambdaLiftState {_funcs} -> state {_funcs = HashMap.insert f (xs, typeOf f, e) _funcs}
  pure f

lambdalift :: MonadIO m => UniqSupply -> ModuleName -> Program (Id Type) -> m (Program (Id Type), HashSet (Id Type))
lambdalift uniqSupply moduleName Program {..} =
  runReaderT ?? LambdaLiftEnv {..} $
    evalStateT ?? LambdaLiftState {_funcs = mempty, _knowns = HashSet.fromList $ map (view _1) topFuns} $ do
      topFuns <- traverse (\(f, ps, t, e) -> (f,ps,t,) <$> llift e) topFuns
      funcs <>= HashMap.fromList (map (\(f, ps, t, e) -> (f, (ps, t, e))) topFuns)
      knowns <>= HashSet.fromList (map (view _1) topFuns)
      LambdaLiftState {_funcs} <- get
      -- TODO: lambdalift topVars
      prog <-
        flat $
          Program
            topVars
            ( map (\(f, (ps, t, e)) -> (f, ps, t, e)) $
                HashMap.toList _funcs
            )
            extFuns
      prog <- appProgram toDirect prog
      (prog,) <$> use knowns

llift :: (MonadIO f, MonadState LambdaLiftState f, MonadReader LambdaLiftEnv f) => Exp (Id Type) -> f (Exp (Id Type))
llift (Atom a) = pure $ Atom a
llift (Call (Var f) xs) = do
  ks <- use knowns
  if f `member` ks then pure $ CallDirect f xs else pure $ Call (Var f) xs
llift (Call f xs) = pure $ Call f xs
llift (CallDirect f xs) = pure $ CallDirect f xs
llift (RawCall f t xs) = pure $ RawCall f t xs
llift (BinOp op x y) = pure $ BinOp op x y
llift (Cast t x) = pure $ Cast t x
llift (Let [LocalDef n t (Fun xs call@Call {})] e) = do
  call' <- llift call
  Let [LocalDef n t (Fun xs call')] <$> llift e
llift (Let [LocalDef n t o@(Fun _ RawCall {})] e) = Let [LocalDef n t o] <$> llift e
llift (Let [LocalDef n t o@(Fun _ CallDirect {})] e) = Let [LocalDef n t o] <$> llift e
llift (Let [LocalDef n t (Fun as body)] e) = do
  backup <- get
  ks <- use knowns
  -- nがknownだと仮定してlambda liftする
  knowns . at n ?= ()
  body' <- llift body
  funcs . at n ?= (as, t, body')
  (e', state) <- localState $ llift e
  -- (Fun as body')の自由変数がknownsを除いてなく、e'の自由変数にnが含まれないならnはknown
  -- (Call n _)は(CallDirect n _)に変換されているので、nが値として使われているときのみ自由変数になる
  let fvs = HashSet.difference (freevars body') (ks <> HashSet.fromList as)
  if null fvs && not (n `member` freevars e')
    then do
      put state
      pure e'
    else do
      put backup
      body' <- llift body
      let fvs = HashSet.difference (freevars body') (ks <> HashSet.fromList as)
      newFun <- def n (toList fvs <> as) body'
      Let [LocalDef n t (Fun as (CallDirect newFun $ map Var $ toList fvs <> as))] <$> llift e
llift (Let ds e) = Let ds <$> llift e
llift (Match e cs) = Match <$> llift e <*> traverseOf (traversed . appCase) llift cs
llift (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) llift cs <*> llift e
llift (SwitchUnboxed a cs e) = SwitchUnboxed a <$> traverseOf (traversed . _2) llift cs <*> llift e
llift (Destruct a c xs e) = Destruct a c xs <$> llift e
llift (DestructRecord a xs e) = DestructRecord a xs <$> llift e
llift (Assign x v e) = Assign x <$> llift v <*> llift e
llift (Error t) = pure $ Error t

-- | `toDirect` converts `Call` to `CallDirect` if the callee is known.
-- If `f` is a known function, we must call it directly.
-- These conversions are almost done in `llift`, but not all of them.
toDirect :: (MonadIO f, MonadState LambdaLiftState f, MonadReader LambdaLiftEnv f) => Exp (Id Type) -> f (Exp (Id Type))
toDirect (Atom a) = pure $ Atom a
toDirect (Call (Var f) xs) = do
  ks <- use knowns
  if f `member` ks then pure $ CallDirect f xs else pure $ Call (Var f) xs
toDirect (Call f xs) = pure $ Call f xs
toDirect (CallDirect f xs) = pure $ CallDirect f xs
toDirect (RawCall f t xs) = pure $ RawCall f t xs
toDirect (BinOp op x y) = pure $ BinOp op x y
toDirect (Cast t x) = pure $ Cast t x
toDirect (Let ds e) = Let <$> traverseOf (traversed . object . appObj) toDirect ds <*> toDirect e
toDirect (Match e cs) = Match <$> toDirect e <*> traverseOf (traversed . appCase) toDirect cs
toDirect (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) toDirect cs <*> toDirect e
toDirect (SwitchUnboxed a cs e) = SwitchUnboxed a <$> traverseOf (traversed . _2) toDirect cs <*> toDirect e
toDirect (Destruct a c xs e) = Destruct a c xs <$> toDirect e
toDirect (DestructRecord a xs e) = DestructRecord a xs <$> toDirect e
toDirect (Assign x v e) = Assign x <$> toDirect v <*> toDirect e
toDirect (Error t) = pure $ Error t
