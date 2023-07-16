-- | Normalization for Core using delimited continuations.
module Koriel.Core.Flat (normalize, normalizeExpr) where

import Control.Lens (has, traverseOf, traversed, _2)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (..), evalContT, shiftT)
import Data.Traversable (for)
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq (Uniq)
import Koriel.Prelude

normalize ::
  (State Uniq :> es, Reader ModuleName :> es) =>
  Program (Id Type) ->
  Eff es (Program (Id Type))
normalize Program {..} = do
  topVars <- for topVars \(name, ty, expr) -> do
    expr' <- runContT (flat expr) pure
    pure (name, ty, expr')
  topFuns <- for topFuns \(name, params, ty, expr) -> do
    expr' <- runContT (flat expr) pure
    pure (name, params, ty, expr')
  pure Program {..}

normalizeExpr ::
  (State Uniq :> es, Reader ModuleName :> es) =>
  Expr (Id Type) ->
  Eff es (Expr (Id Type))
normalizeExpr e = runContT (flat e) pure

-- Traverse the expression tree.
flat ::
  (State Uniq :> es, Reader ModuleName :> es) =>
  Expr (Id Type) ->
  ContT (Expr (Id Type)) (Eff es) (Expr (Id Type))
flat e@Atom {} = pure e
flat e@Call {} = pure e
flat e@CallDirect {} = pure e
flat e@RawCall {} = pure e
flat e@Cast {} = pure e
flat (Let ds e) = shiftT \k -> do
  ds <- lift $ traverse flatLocalDef ds
  e <- inScope k $ flat e
  pure $ Let ds e
flat (Match scr cs) = shiftT \k -> do
  scr <- flat scr
  cs <- traverse (flatCase k) cs
  scr' <- lift $ newTemporalId "scrutinee" (typeOf scr)
  pure $ assign scr' scr $ matchToSwitch scr' cs
flat (Switch v cs e) = shiftT \k -> lift do
  cs <- traverseOf (traversed . _2) ?? cs $ \e ->
    runContT (flat e) k
  e <- runContT (flat e) k
  pure $ Switch v cs e
flat (SwitchUnboxed v cs e) = shiftT \k -> lift do
  cs <- traverseOf (traversed . _2) ?? cs $ \e ->
    runContT (flat e) k
  e <- runContT (flat e) k
  pure $ SwitchUnboxed v cs e
flat (Destruct v con xs e) = shiftT \k -> do
  e <- inScope k $ flat e
  pure $ Destruct v con xs e
flat (DestructRecord v kvs e) = shiftT \k -> do
  e <- inScope k $ flat e
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
  (State Uniq :> es, Reader ModuleName :> es) =>
  (Expr (Id Type) -> Eff es (Expr (Id Type))) ->
  Case (Id Type) ->
  ContT r' (Eff es) (Case (Id Type))
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
      defaultCase = maybe (Error $ typeOf c) (bindToAssign scr) (find (has _Bind) cs)
   in Switch (Var scr) cs' defaultCase
  where
    unpackToDestruct scr (Unpack con@(Con tag _) xs e) = (tag, Destruct (Var scr) con xs e)
    unpackToDestruct _ _ = error "unpackToDestruct: unreachable"
matchToSwitch scr (OpenRecord kvs e : _) =
  DestructRecord (Var scr) kvs e
matchToSwitch scr cs@(c@Exact {} : _) =
  let cs' = map flatExact $ takeWhile (has _Exact) cs
      defaultCase = maybe (Error $ typeOf c) (bindToAssign scr) (find (has _Bind) cs)
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
  (State Uniq :> es, Reader ModuleName :> es) =>
  LocalDef (Id Type) ->
  Eff es (LocalDef (Id Type))
flatLocalDef (LocalDef var ty obj) = LocalDef var ty <$> flatObj obj

flatObj ::
  (State Uniq :> es, Reader ModuleName :> es) =>
  Obj (Id Type) ->
  Eff es (Obj (Id Type))
flatObj (Fun ps e) =
  -- eがFunを飛び出ないようresetする
  Fun ps <$> evalContT (flat e)
flatObj o = pure o

inScope ::
  (Monad m) =>
  -- | continuation
  (a -> m r) ->
  -- | computation must be in the scope
  -- This computation do not escape from `scope`.
  ContT r m a ->
  ContT r' m r
inScope k e =
  {-
    resetT $ lift . k =<< e
  = lift . evalContT $ lift . k =<< e -- apply 'resetT'
  = (\m -> ContT (m >>=)) . evalContT $ (\m -> ContT (m >>=)) . k =<< e -- apply 'lift'
  = (\m -> ContT (m >>=)) $ evalContT $ (\m -> ContT (m >>=)) . k =<< e -- f . g $ x -> f $ g $ x
  = ContT ((evalContT $ (\m -> ContT (m >>=)) . k =<< e) >>=) -- apply first '$'
  = lift (evalContT $ (\m -> ContT (m >>=)) . k =<< e) -- unapply 'lift'
  = lift (evalContT $ e >>= (\m -> ContT (m >>=)) . k) -- f =<< m -> m >>= f
  = lift (evalContT $ e >>= (\x -> (\m -> ContT (m >>=)) . k $ x)) -- eta expansion
  = lift (evalContT $ e >>= (\x -> (\m -> ContT (m >>=)) $ k $ x)) --  f . g $ x -> f $ g $ x
  = lift (evalContT $ e >>= (\x -> ContT (k x >>=))) -- apply second and third '$'
  = lift (evalContT (ContT \c -> runContT e (\x -> runContT ((\x -> ContT (k x >>=)) x) c))) -- apply first '>>='
  = lift (runContT (ContT \c -> runContT e (\x -> runContT ((\x -> ContT (k x >>=)) x) c)) pure) -- apply 'evalContT'
  = lift (runContT e (\x -> k x >>= pure)) -- apply some functions as same as above
  = lift (runContT e (\x -> k x)) -- m >>= pure = m
  = lift (runContT e k) -- eta reduction

  This conversion can be derived from the type.
  With 'k :: a -> m r' and 'e :: ContT r m a', we can get 'runContT e k :: m r'.
  Then we can get 'lift $ runContT e k :: ContT r' m r'.
  -}
  lift $ runContT e k

assign :: Id Type -> Expr (Id Type) -> Expr (Id Type) -> Expr (Id Type)
assign x v (Atom (Var y)) | x == y = v
assign x (Atom v) e = replaceOf atom (Var x) v e
assign x v e = Assign x v e
