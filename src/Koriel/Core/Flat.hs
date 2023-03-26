module Koriel.Core.Flat
  ( flat,
    runFlat,
    flatExp,
  )
where

import Control.Lens (traverseOf, traversed, _2)
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id (HasModuleName, Id, ModuleName, newTemporalId)
import Koriel.MonadUniq (HasUniqSupply, UniqSupply)
import Koriel.Prelude

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
flatExp (Destruct v con ps e) = Destruct v con ps <$> runFlat (flatExp e)
flatExp (Assign x v e) = do
  v <- flatExp v
  case v of
    Atom (Var v') ->
      -- Flatten e[x := v'] after assigning v' to x.
      -- If we do it before, we might miss some assignments to x.
      -- These assignments are constructed by the Endo monoid.
      flatExp $ replaceOf atom (Var x) (Var v') e
    _ -> do
      tell $ Endo $ \k -> Assign x v k
      flatExp e
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
  e' <- runFlat $ flatExp e'
  v <- newTemporalId (nameFromCon con) (typeOf e)
  pure $ Assign v e (Destruct (Var v) con ps e')
  where
    nameFromCon :: Con -> Text
    nameFromCon (Con (Data name) _) = name
    nameFromCon (Con Tuple _) = "tuple"
flatMatch e [Bind x _ e'] = do
  e <- flatExp e
  e' <- runFlat $ flatExp e'
  pure $ Assign x e e'
flatMatch e cs = Match <$> flatExp e <*> traverseOf (traversed . appCase) (runFlat . flatExp) cs