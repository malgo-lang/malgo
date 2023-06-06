-- | Normalization for Core using delimited continuations.
module Koriel.Core.FlatDC (normalize) where

import Control.Lens (traverseOf, traversed, _2)
import Control.Monad.Trans.Cont (ContT (runContT), resetT, shiftT)
import Data.Traversable (for)
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.Prelude

normalize :: Monad m => Program (Id Type) -> m (Program (Id Type))
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
flat :: Monad m => Expr (Id Type) -> ContT (Expr (Id Type)) m (Expr (Id Type))
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
flat (Match e cs) = shiftT \k -> do
  e <- flat e
  cs <- traverse (flatCase k) cs
  pure $ Match e cs
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
  let builder = case v of
        Atom v -> \e -> do
          -- 自明な束縛は畳み込む
          replaceOf atom (Var x) v e
        v -> Assign x v
  -- eの中の式は、必ず(= x v [.])の中に現れないといけない。
  -- そのため、resetする。
  -- (= x v e)の外の式を持ち込むため、shiftしたkを適用する。
  e <- inScope k $ flat e
  pure $ builder e
flat e@Error {} = pure e

flatCase :: Monad m => (Expr (Id Type) -> m (Expr (Id Type))) -> Case (Id Type) -> ContT r' m (Case (Id Type))
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

flatLocalDef :: Monad m => LocalDef (Id Type) -> ContT (Expr (Id Type)) m (LocalDef (Id Type))
flatLocalDef (LocalDef var ty obj) = LocalDef var ty <$> flatObj obj

flatObj :: Monad m => Obj (Id Type) -> ContT (Expr (Id Type)) m (Obj (Id Type))
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
