-- | Normalization for Core using delimited continuations.
module Koriel.Core.FlatDC (normalize) where

import Control.Lens (has, traverseOf, traversed, _2)
import Control.Monad.Trans.Cont (ContT (runContT), shiftT)
import Data.Traversable (for)
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq (HasUniqSupply)
import Koriel.Prelude

normalize ::
  ( MonadReader env m,
    MonadIO m,
    HasUniqSupply env,
    HasModuleName env
  ) =>
  Program (Id Type) ->
  m (Program (Id Type))
normalize Program {..} = do
  topVars <- for topVars \(name, ty, expr) -> do
    expr' <- runContT (flat expr) pure
    pure (name, ty, expr')
  topFuns <- for topFuns \(name, params, ty, expr) -> do
    expr' <- runContT (flat expr) pure
    pure (name, params, ty, expr')
  pure Program {..}

-- Traverse the expression tree.
-- TODO: Implement the normalization.
flat ::
  ( MonadReader env m,
    MonadIO m,
    HasUniqSupply env,
    HasModuleName env
  ) =>
  Expr (Id Type) ->
  ContT (Expr (Id Type)) m (Expr (Id Type))
flat e@Atom {} = pure e
flat e@Call {} = pure e
flat e@CallDirect {} = pure e
flat e@RawCall {} = pure e
flat e@BinOp {} = pure e
flat e@Cast {} = pure e
flat (Let ds e) = shiftT \k -> do
  ds <- traverse flatLocalDef ds
  e <- inScope k $ flat e
  pure $ Let ds e
flat (Match scr cs) = shiftT \k -> do
  scr <- flat scr
  cs <- traverse (flatCase k) cs
  scr' <- newTemporalId "scrutinee" (typeOf scr)
  pure $ assign scr' scr $ matchToSwitch scr' cs
flat (Switch v cs e) = shiftT \k -> do
  cs <- traverseOf (traversed . _2) ?? cs $ \e ->
    inScope k $ flat e
  e <- flat e
  pure $ Switch v cs e
flat (SwitchUnboxed v cs e) = shiftT \k -> do
  cs <- traverseOf (traversed . _2) ?? cs $ \e ->
    inScope k $ flat e
  e <- flat e
  pure $ SwitchUnboxed v cs e
flat (Destruct v con xs e) = do
  e <- flat e
  pure $ Destruct v con xs e
flat (DestructRecord v kvs e) = do
  e <- flat e
  pure $ DestructRecord v kvs e
flat (Assign x v e) = shiftT \k -> do
  v <- flat v
  -- eの中の式は、必ず(= x v [.])の中に現れないといけない。
  -- そのため、resetする。
  -- (= x v e)の外の式を持ち込むため、shiftしたkを適用する。
  e <- inScope k $ flat e
  pure $ assign x v e
flat e@Error {} = pure e

flatCase ::
  ( MonadReader env m,
    MonadIO m,
    HasUniqSupply env,
    HasModuleName env
  ) =>
  (Expr (Id Type) -> m (Expr (Id Type))) ->
  Case (Id Type) ->
  ContT r' m (Case (Id Type))
flatCase k (Unpack con as e) = do
  e <- inScope k $ flat e
  pure $ Unpack con as e
flatCase k (OpenRecord kvs e) = do
  e <- inScope k $ flat e
  pure $ OpenRecord kvs e
flatCase k (Exact u e) = do
  e <- inScope k $ flat e
  pure $ Exact u e
flatCase k (Bind n t e) = do
  e <- inScope k $ flat e
  pure $ Bind n t e

matchToSwitch :: Id Type -> [Case (Id Type)] -> Expr (Id Type)
matchToSwitch scr cs@(c@Unpack {} : _) =
  let cs' = map (unpackToDestruct scr) $ takeWhile (has _Unpack) cs
      defaultCase = fromMaybe (Error $ typeOf c) $ bindToAssign scr <$> find (has _Bind) cs
   in Switch (Var scr) cs' defaultCase
  where
    unpackToDestruct scr (Unpack con@(Con tag _) xs e) = (tag, Destruct (Var scr) con xs e)
    unpackToDestruct _ _ = error "unpackToDestruct: unreachable"
matchToSwitch scr (OpenRecord kvs e : _) =
  DestructRecord (Var scr) kvs e
matchToSwitch scr cs@(c@Exact {} : _) =
  let cs' = map flatExact $ takeWhile (has _Exact) cs
      defaultCase = fromMaybe (Error $ typeOf c) $ bindToAssign scr <$> find (has _Bind) cs
   in SwitchUnboxed (Var scr) cs' defaultCase
  where
    flatExact (Exact u e) = (u, e)
    flatExact _ = error "flatExact: unreachable"
matchToSwitch scr (Bind x _ e : _) = assign x (Atom $ Var scr) e
matchToSwitch _ [] = error "matchToSwitch: unreachable"

bindToAssign :: Id Type -> Case (Id Type) -> Expr (Id Type)
bindToAssign scr (Bind x _ e) = assign x (Atom $ Var scr) e
bindToAssign _ _ = error "bindToAssign: unreachable"

flatLocalDef ::
  ( MonadReader env m,
    MonadIO m,
    HasUniqSupply env,
    HasModuleName env
  ) =>
  LocalDef (Id Type) ->
  ContT (Expr (Id Type)) m (LocalDef (Id Type))
flatLocalDef (LocalDef var ty obj) = LocalDef var ty <$> flatObj obj

flatObj ::
  ( MonadReader env m,
    MonadIO m,
    HasUniqSupply env,
    HasModuleName env
  ) =>
  Obj (Id Type) ->
  ContT (Expr (Id Type)) m (Obj (Id Type))
flatObj (Fun ps e) =
  -- eがFunを飛び出ないようresetする
  Fun ps <$> inScope pure (flat e)
flatObj o = pure o

inScope ::
  Monad m =>
  -- | continuation
  (a -> m r) ->
  -- | computation must be in the scope
  -- This computation do not escape from `scope`.
  ContT r m a ->
  ContT r' m r
inScope k e =
  -- resetT $ lift . k =<< e
  lift $ runContT e k

assign :: Id Type -> Expr (Id Type) -> Expr (Id Type) -> Expr (Id Type)
assign x v (Atom (Var y)) | x == y = v
assign x (Atom v) e = replaceOf atom (Var x) v e
assign x v e = Assign x v e