{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}

module Malgo.Core.LambdaLift
  ( lambdalift,
  )
where

import Control.Lens (traverseOf, traversed, view, _1, _2)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Malgo.Core.Flat (normalize)
import Malgo.Core.Syntax
import Malgo.Core.Type
import Malgo.Id
import Malgo.Module
import Malgo.MonadUniq
import Malgo.Prelude

data LambdaLiftState = LambdaLiftState
  { funcs :: Map (Meta Type) ([Meta Type], Type, Expr (Meta Type)),
    -- | Known functions. If a function is known, that function can be called directly.
    knowns :: Set (Meta Type),
    -- | Variables that defined in global scope.
    defined :: Set (Meta Type)
  }

-- | Add a function to the state.
-- It does not use lens.
addFunc :: (State LambdaLiftState :> es) => Meta Type -> ([Meta Type], Type, Expr (Meta Type)) -> Eff es ()
addFunc f x = modify $ \state@LambdaLiftState {funcs} -> state {funcs = Map.insert f x funcs}

-- | Add a known function to the state.
-- It does not use lens.
addKnown :: (State LambdaLiftState :> es) => Meta Type -> Eff es ()
addKnown f = modify $ \state@LambdaLiftState {knowns} -> state {knowns = Set.insert f knowns}

isKnown :: (State LambdaLiftState :> es) => Meta Type -> Eff es Bool
isKnown f = do
  ks <- gets @LambdaLiftState (.knowns)
  pure $ f `Set.member` ks

def :: (State LambdaLiftState :> es, Reader ModuleName :> es, State Uniq :> es) => Meta Type -> [Meta Type] -> Expr (Meta Type) -> Eff es (Meta Type)
def name xs e = do
  f <- withMeta (map typeOf xs :-> typeOf e) <$> newTemporalId ("raw_" <> name.id.name)
  -- knowns . at f ?= ()
  addFunc f (xs, typeOf f, e)
  pure f

-- | Lambda lifting
lambdalift ::
  (Reader ModuleName :> es, State Uniq :> es) =>
  Program (Meta Type) ->
  Eff es (Program (Meta Type))
lambdalift Program {..} =
  evalState
    LambdaLiftState {funcs = mempty, knowns = Set.fromList $ map (view _1) topFuns, defined = Set.fromList $ map (view _1) topFuns <> map (view _1) topVars}
    $ do
      topFuns <- traverse (\(f, ps, t, e) -> (f,ps,t,) <$> llift e) topFuns
      for_ topFuns \(f, ps, t, e) -> do
        addFunc f (ps, t, e)
        addKnown f
      LambdaLiftState {funcs} <- get
      -- TODO: lambdalift topVars
      prog <-
        normalize
          $ Program
            topVars
            ( map (\(f, (ps, t, e)) -> (f, ps, t, e))
                $ Map.toList funcs
            )
            extFuns
      traverseOf expr toDirect prog

llift ::
  ( State LambdaLiftState :> es,
    Reader ModuleName :> es,
    State Uniq :> es
  ) =>
  Expr (Meta Type) ->
  Eff es (Expr (Meta Type))
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
  backup <- get @LambdaLiftState
  ks <- gets @LambdaLiftState (.knowns)
  -- nがknownだと仮定してlambda liftする
  addKnown n
  body' <- llift body
  addFunc n (as, t, body')
  (e', state) <- do
    s <- get @LambdaLiftState
    e' <- llift e
    s' <- get @LambdaLiftState
    put s
    pure (e', s')
  -- (Fun as body')の自由変数がknownsを除いてなく、e'の自由変数にnが含まれないならnはknown
  -- (Call n _)は(CallDirect n _)に変換されているので、nが値として使われているときのみ自由変数になる
  defined <- gets @LambdaLiftState (.defined)
  let fvs = Set.difference (freevars body') (ks <> defined <> Set.fromList as)
  if null fvs && not (n `Set.member` freevars e')
    then do
      put state
      pure e'
    else do
      put backup
      body' <- llift body
      defined <- gets @LambdaLiftState (.defined)
      let fvs = Set.difference (freevars body') (ks <> defined <> Set.fromList as)
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
toDirect :: (State LambdaLiftState :> es) => Expr (Meta Type) -> Eff es (Expr (Meta Type))
toDirect (Atom a) = pure $ Atom a
toDirect (Call (Var f) xs) = do
  ks <- gets @LambdaLiftState (.knowns)
  if f `Set.member` ks then pure $ CallDirect f xs else pure $ Call (Var f) xs
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
