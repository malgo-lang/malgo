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

flat :: (MonadIO m, MonadReader env m, HasUniqSupply env) => ModuleName -> Program (Id Type) -> m (Program (Id Type))
flat moduleName prog = do
  uniqSupply <- asks (.uniqSupply)
  runReaderT ?? FlatEnv {..} $
    traverseOf appProgram (runFlat . flatExp) prog

runFlat :: Monad m => WriterT (Endo r) m r -> m r
runFlat = fmap (uncurry (flip appEndo)) . runWriterT

flatExp ::
  ( MonadReader env m,
    MonadIO m,
    HasUniqSupply env,
    HasModuleName env
  ) =>
  Exp (Id Type) ->
  WriterT (Endo (Exp (Id Type))) m (Exp (Id Type))
flatExp (Let ds e) = do
  tell . Endo . Let =<< traverseOf (traversed . object . appObj) (runFlat . flatExp) ds
  flatExp e
flatExp (Match v [Unpack con ps e]) = do
  v <- flatExp v
  v' <- newTemporalId (nameFromCon con) (typeOf v)
  tell $ Endo $ \k -> Assign v' v (Destruct (Var v') con ps k) -- Match v [Unpack con ps k]
  flatExp e
  where
    nameFromCon :: Con -> Text
    nameFromCon (Con (Data name) _) = name
    nameFromCon (Con Tuple _) = "tuple"
flatExp (Match v [Bind x _ e]) = do
  v <- flatExp v
  tell $ Endo $ \k -> Assign x v k
  flatExp e
flatExp (Match e cs) =
  Match <$> flatExp e <*> traverseOf (traversed . appCase) (runFlat . flatExp) cs
flatExp (Switch v cs) = Switch v <$> traverseOf (traversed . _2) (runFlat . flatExp) cs
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
