module Koriel.Core.Flat
  ( flat,
    runFlat,
    flatExp,
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

flat :: (MonadIO m, MonadReader env m, HasUniqSupply env, HasModuleName env) => Program (Id Type) -> m (Program (Id Type))
flat prog = do
  uniqSupply <- asks (.uniqSupply)
  moduleName <- asks (.moduleName)
  runReaderT ?? FlatEnv {..} $
    traverseOf appProgram (runFlat . flatExp) prog

type FlatT m = WriterT (Endo (Exp (Id Type))) m

runFlat :: Monad m => FlatT m (Exp (Id Type)) -> m (Exp (Id Type))
runFlat = fmap (uncurry (flip appEndo)) . runWriterT

flatExp ::
  ( MonadReader env m,
    MonadIO m,
    HasUniqSupply env,
    HasModuleName env
  ) =>
  Exp (Id Type) ->
  FlatT m (Exp (Id Type))
flatExp (Let ds e) = do
  tell . Endo . Let =<< traverseOf (traversed . object . appObj) (runFlat . flatExp) ds
  flatExp e
flatExp (Match e cs) = flatMatch e cs
flatExp (Switch v cs e) = Switch v <$> traverseOf (traversed . _2) (runFlat . flatExp) cs <*> flatExp e
flatExp (Destruct v con ps e) = do
  tell $ Endo $ \k -> Destruct v con ps k
  flatExp e
flatExp (DestructRecord v kvs e) = do
  tell $ Endo $ \k -> DestructRecord v kvs k
  flatExp e
flatExp (Assign x (Atom (Var v)) e) = do
  flatExp $ replaceOf atom (Var x) (Var v) e
flatExp (Assign x v e) = do
  v <- flatExp v
  tell $ Endo $ \k -> Assign x v k
  flatExp e
-- v <- flatExp v
-- case v of
--   Atom (Var v') ->
--     -- Flatten e[x := v'] after assigning v' to x.
--     -- If we do it before, we might miss some assignments to x.
--     -- These assignments are constructed by the Endo monoid.
--     flatExp $ replaceOf atom (Var x) (Var v') e
--   _ -> do
--     tell $ Endo $ \k ->
--       case k of
--         Atom (Var y) | x == y -> v -- (= x v x) -> v
--         _ -> Assign x v k
--     flatExp e
flatExp e = pure e

flatMatch ::
  ( MonadReader env m,
    MonadIO m,
    HasUniqSupply env,
    HasModuleName env
  ) =>
  Exp (Id Type) ->
  [Case (Id Type)] ->
  FlatT m (Exp (Id Type))
flatMatch e [Unpack con ps e'] = do
  e <- flatExp e
  v <- newTemporalId (nameFromCon con) (typeOf e)
  tell $ Endo $ \k -> Assign v e k
  tell $ Endo $ \k -> Destruct (Var v) con ps k
  flatExp e'
  where
    nameFromCon :: Con -> Text
    nameFromCon (Con (Data name) _) = name
    nameFromCon (Con Tuple _) = "tuple"
flatMatch e [Bind x _ e'] = do
  e <- flatExp e
  tell $ Endo $ \k ->
    case k of
      Atom (Var y) | x == y -> e -- (= x e x) -> v
      _ -> Assign x e k
  flatExp e'
flatMatch e cs = do
  e <- flatExp e
  cs <- traverseOf (traversed . appCase) (runFlat . flatExp) cs
  e' <- newTemporalId "match" (typeOf e)
  tell $ Endo $ \k -> Assign e' e k
  matchToSwitch e' cs

matchToSwitch :: Monad m => Id Type -> [Case (Id Type)] -> FlatT m (Exp (Id Type))
matchToSwitch scrutinee cs@(Unpack {} : _) = do
  let cs' = map (unpackToDestruct scrutinee) $ takeWhile (has _Unpack) cs
  let mdef = bindToAssign scrutinee <$> find (has _Bind) cs
  let def = case mdef of
        Just def -> def
        Nothing -> Error (typeOf $ snd $ Unsafe.head cs')
  pure $ Switch (Var scrutinee) cs' def
  where
    unpackToDestruct :: Id Type -> Case (Id Type) -> (Tag, Exp (Id Type))
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
    flatExact :: Case a -> (Unboxed, Exp a)
    flatExact (Exact u e) = (u, e)
    flatExact _ = error "flatExact: unreachable"
matchToSwitch scrutinee (Bind x _ e : _) = pure $ Assign x (Atom $ Var scrutinee) e
matchToSwitch _ _ = error "matchToSwitch: unreachable"

bindToAssign :: Id Type -> Case (Id Type) -> Exp (Id Type)
bindToAssign s (Bind x _ e) = Assign x (Atom (Var s)) e
bindToAssign _ _ = error "bindToAssign: unreachable"
