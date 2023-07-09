{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}

module Koriel.Core.LambdaLift (
  lambdalift,
)
where

import Control.Lens (traverseOf, traversed, view, _1, _2)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Koriel.Core.Flat (normalize)
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude

data LambdaLiftState = LambdaLiftState
  { funcs :: HashMap (Id Type) ([Id Type], Type, Expr (Id Type)),
    -- | Known functions. If a function is known, that function can be called directly.
    knowns :: HashSet (Id Type),
    -- | Variables that defined in global scope.
    defined :: HashSet (Id Type)
  }

-- | Add a function to the state.
-- It does not use lens.
addFunc :: MonadState LambdaLiftState m => Id Type -> ([Id Type], Type, Expr (Id Type)) -> m ()
addFunc f x = modify $ \state@LambdaLiftState {funcs} -> state {funcs = HashMap.insert f x funcs}

-- | Add a known function to the state.
-- It does not use lens.
addKnown :: MonadState LambdaLiftState m => Id Type -> m ()
addKnown f = modify $ \state@LambdaLiftState {knowns} -> state {knowns = HashSet.insert f knowns}

isKnown :: MonadState LambdaLiftState m => Id Type -> m Bool
isKnown f = do
  ks <- gets (.knowns)
  pure $ f `HashSet.member` ks

data LambdaLiftEnv = LambdaLiftEnv
  { uniqSupply :: UniqSupply,
    moduleName :: ModuleName
  }

data DefEnv = DefEnv {uniqSupply :: UniqSupply, moduleName :: ModuleName}

def :: (MonadIO m, MonadState LambdaLiftState m, MonadReader LambdaLiftEnv m) => Id Type -> [Id Type] -> Expr (Id Type) -> m (Id Type)
def name xs e = do
  uniqSupply <- asks (.uniqSupply)
  f <- runReaderT (newTemporalId ("raw_" <> name.name) (map typeOf xs :-> typeOf e)) (DefEnv uniqSupply name.moduleName)
  -- knowns . at f ?= ()
  addFunc f (xs, typeOf f, e)
  pure f

-- | Lambda lifting
lambdalift :: MonadIO m => UniqSupply -> ModuleName -> Program (Id Type) -> m (Program (Id Type))
lambdalift uniqSupply moduleName Program {..} =
  runReaderT
    ?? LambdaLiftEnv {..}
    $ evalStateT
      ?? LambdaLiftState {funcs = mempty, knowns = HashSet.fromList $ map (view _1) topFuns, defined = HashSet.fromList $ map (view _1) topFuns <> map (view _1) topVars}
    $ do
      topFuns <- traverse (\(f, ps, t, e) -> (f,ps,t,) <$> llift e) topFuns
      for_ topFuns \(f, ps, t, e) -> do
        addFunc f (ps, t, e)
        addKnown f
      LambdaLiftState {funcs} <- get
      -- TODO: lambdalift topVars
      prog <-
        normalize $
          Program
            topVars
            ( map (\(f, (ps, t, e)) -> (f, ps, t, e)) $
                HashMap.toList funcs
            )
            extFuns
      traverseOf expr toDirect prog

llift :: (MonadIO f, MonadState LambdaLiftState f, MonadReader LambdaLiftEnv f) => Expr (Id Type) -> f (Expr (Id Type))
llift (Atom a) = pure $ Atom a
llift (Call (Var f) xs) = do
  ifM (isKnown f) (pure $ CallDirect f xs) (pure $ Call (Var f) xs)
llift (Call f xs) = pure $ Call f xs
llift (CallDirect f xs) = pure $ CallDirect f xs
llift (RawCall f t xs) = pure $ RawCall f t xs
llift (Cast t x) = pure $ Cast t x
llift (Let [LocalDef n t (Fun xs call@Call {})] e) = do
  call' <- llift call
  Let [LocalDef n t (Fun xs call')] <$> llift e
llift (Let [LocalDef n t o@(Fun _ RawCall {})] e) = Let [LocalDef n t o] <$> llift e
llift (Let [LocalDef n t o@(Fun _ CallDirect {})] e) = Let [LocalDef n t o] <$> llift e
llift (Let [LocalDef n t (Fun as body)] e) = do
  backup <- get
  ks <- gets (.knowns)
  -- nがknownだと仮定してlambda liftする
  addKnown n
  body' <- llift body
  addFunc n (as, t, body')
  (e', state) <- localState $ llift e
  -- (Fun as body')の自由変数がknownsを除いてなく、e'の自由変数にnが含まれないならnはknown
  -- (Call n _)は(CallDirect n _)に変換されているので、nが値として使われているときのみ自由変数になる
  defined <- gets (.defined)
  let fvs = HashSet.difference (freevars body') (ks <> defined <> HashSet.fromList as)
  if null fvs && not (n `HashSet.member` freevars e')
    then do
      put state
      pure e'
    else do
      put backup
      body' <- llift body
      defined <- gets (.defined)
      let fvs = HashSet.difference (freevars body') (ks <> defined <> HashSet.fromList as)
      newFun <- def n (toList fvs <> as) body'
      Let [LocalDef n t (Fun as (CallDirect newFun $ map Var $ toList fvs <> as))] <$> llift e
llift (Let ds e) = Let ds <$> llift e
llift (Match e cs) = Match <$> llift e <*> traverseOf (traversed . expr) llift cs
llift (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) llift cs <*> llift e
llift (SwitchUnboxed a cs e) = SwitchUnboxed a <$> traverseOf (traversed . _2) llift cs <*> llift e
llift (Destruct a c xs e) = Destruct a c xs <$> llift e
llift (DestructRecord a xs e) = DestructRecord a xs <$> llift e
llift (Assign x v e) = Assign x <$> llift v <*> llift e
llift (Error t) = pure $ Error t

-- | `toDirect` converts `Call` to `CallDirect` if the callee is known.
-- If `f` is a known function, we must call it directly.
-- These conversions are almost done in `llift`, but not all of them.
toDirect :: (MonadIO f, MonadState LambdaLiftState f, MonadReader LambdaLiftEnv f) => Expr (Id Type) -> f (Expr (Id Type))
toDirect (Atom a) = pure $ Atom a
toDirect (Call (Var f) xs) = do
  ks <- gets (.knowns)
  if f `HashSet.member` ks then pure $ CallDirect f xs else pure $ Call (Var f) xs
toDirect (Call f xs) = pure $ Call f xs
toDirect (CallDirect f xs) = pure $ CallDirect f xs
toDirect (RawCall f t xs) = pure $ RawCall f t xs
toDirect (Cast t x) = pure $ Cast t x
toDirect (Let ds e) = Let <$> traverseOf (traversed . expr) toDirect ds <*> toDirect e
toDirect (Match e cs) = Match <$> toDirect e <*> traverseOf (traversed . expr) toDirect cs
toDirect (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) toDirect cs <*> toDirect e
toDirect (SwitchUnboxed a cs e) = SwitchUnboxed a <$> traverseOf (traversed . _2) toDirect cs <*> toDirect e
toDirect (Destruct a c xs e) = Destruct a c xs <$> toDirect e
toDirect (DestructRecord a xs e) = DestructRecord a xs <$> toDirect e
toDirect (Assign x v e) = Assign x <$> toDirect v <*> toDirect e
toDirect (Error t) = pure $ Error t
