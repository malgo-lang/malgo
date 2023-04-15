module Koriel.Core.Flat
  ( flat,
    runFlat,
    flatExpr,
  )
where

import Control.Lens (has, traverseOf, traversed, _2)
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id (HasModuleName, Id, ModuleName, newTemporalId)
import Koriel.MonadUniq (HasUniqSupply, UniqSupply)
import Koriel.Prelude
import Relude.Unsafe qualified as Unsafe

data FlatEnv = FlatEnv
  { uniqSupply :: UniqSupply,
    moduleName :: ModuleName
  }

-- | 'flat' flattens nested let bindings and destructuring assignments.
flat :: (MonadIO m, MonadReader env m, HasUniqSupply env, HasModuleName env) => Program (Id Type) -> m (Program (Id Type))
flat prog = do
  uniqSupply <- asks (.uniqSupply)
  moduleName <- asks (.moduleName)
  runReaderT ?? FlatEnv {..} $
    traverseOf appProgram (runFlat . flatExpr) prog

type FlatT m = WriterT (Endo (Expr (Id Type))) m

runFlat :: Monad m => FlatT m (Expr (Id Type)) -> m (Expr (Id Type))
runFlat = fmap (uncurry (flip appEndo)) . runWriterT

flatExpr ::
  ( MonadReader env m,
    MonadIO m,
    HasUniqSupply env,
    HasModuleName env
  ) =>
  Expr (Id Type) ->
  FlatT m (Expr (Id Type))
flatExpr (Let ds e) = do
  tell . Endo . Let =<< traverseOf (traversed . object . appObj) (runFlat . flatExpr) ds
  flatExpr e
flatExpr (Match e cs) = flatMatch e cs
flatExpr (Switch v cs e) = Switch v <$> traverseOf (traversed . _2) (runFlat . flatExpr) cs <*> flatExpr e
flatExpr (Destruct v con ps e) = do
  tell $ Endo $ \k -> Destruct v con ps k
  flatExpr e
flatExpr (DestructRecord v kvs e) = do
  tell $ Endo $ \k -> DestructRecord v kvs k
  flatExpr e
flatExpr (Assign x (Atom (Var v)) e) = do
  flatExpr $ replaceOf atom (Var x) (Var v) e
flatExpr (Assign x v e) = do
  v <- flatExpr v
  tell $ Endo $ \k -> Assign x v k
  flatExpr e
flatExpr e = pure e

-- | 'flatMatch' flattens match expressions into a sequence of assignments and destructuring assignments.
flatMatch ::
  ( MonadReader env m,
    MonadIO m,
    HasUniqSupply env,
    HasModuleName env
  ) =>
  Expr (Id Type) ->
  [Case (Id Type)] ->
  FlatT m (Expr (Id Type))
flatMatch e [Unpack con ps e'] = do
  e <- flatExpr e
  v <- newTemporalId (nameFromCon con) (typeOf e)
  tell $ Endo $ \k -> Assign v e k
  tell $ Endo $ \k -> Destruct (Var v) con ps k
  flatExpr e'
  where
    nameFromCon :: Con -> Text
    nameFromCon (Con (Data name) _) = name
    nameFromCon (Con Tuple _) = "tuple"
flatMatch e [Bind x _ e'] = do
  e <- flatExpr e
  tell $ Endo $ \k ->
    case k of
      Atom (Var y) | x == y -> e -- (= x e x) -> v
      _ -> Assign x e k
  flatExpr e'
flatMatch e cs = do
  e <- flatExpr e
  cs <- traverseOf (traversed . appCase) (runFlat . flatExpr) cs
  e' <- newTemporalId "match" (typeOf e)
  tell $ Endo $ \k -> Assign e' e k
  matchToSwitch e' cs

-- | 'matchToSwitch' converts a match expression into a switch expression.
matchToSwitch :: Monad m => Id Type -> [Case (Id Type)] -> FlatT m (Expr (Id Type))
matchToSwitch scrutinee cs@(Unpack {} : _) = do
  let cs' = map (unpackToDestruct scrutinee) $ takeWhile (has _Unpack) cs
  let mdef = bindToAssign scrutinee <$> find (has _Bind) cs
  let def = case mdef of
        Just def -> def
        Nothing -> Error (typeOf $ snd $ Unsafe.head cs')
  pure $ Switch (Var scrutinee) cs' def
  where
    unpackToDestruct :: Id Type -> Case (Id Type) -> (Tag, Expr (Id Type))
    unpackToDestruct s (Unpack con@(Con tag _) xs e) = (tag, Destruct (Var s) con xs e)
    unpackToDestruct _ _ = error "unpackToDestruct: unreachable"
matchToSwitch scrutinee (OpenRecord kvs e : _) = do
  pure $ DestructRecord (Var scrutinee) kvs e
matchToSwitch scrutinee cs@(Exact {} : _) = do
  let cs' = map flatExact $ takeWhile (has _Exact) cs
  let mdef = bindToAssign scrutinee <$> find (has _Bind) cs
  let def = case mdef of
        Just def -> def
        Nothing -> Error (typeOf $ snd $ Unsafe.head cs')
  pure $ SwitchUnboxed (Var scrutinee) cs' def
  where
    flatExact :: Case a -> (Unboxed, Expr a)
    flatExact (Exact u e) = (u, e)
    flatExact _ = error "flatExact: unreachable"
matchToSwitch scrutinee (Bind x _ e : _) = pure $ Assign x (Atom $ Var scrutinee) e
matchToSwitch _ _ = error "matchToSwitch: unreachable"

-- | 'bindToAssign' converts a bind case into an assignment.
bindToAssign :: Id Type -> Case (Id Type) -> Expr (Id Type)
bindToAssign s (Bind x _ e) = Assign x (Atom (Var s)) e
bindToAssign _ _ = error "bindToAssign: unreachable"
