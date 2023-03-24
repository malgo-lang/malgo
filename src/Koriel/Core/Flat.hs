module Koriel.Core.Flat
  ( flat,
  )
where

import Control.Lens (traverseOf, traversed, _2)
import Koriel.Core.Syntax
import Koriel.Prelude

flat :: Eq a => Exp a -> Exp a
flat e = runIdentity $ runFlat (flatExp e)

runFlat :: Monad m => WriterT (Endo r) m r -> m r
runFlat = fmap (uncurry (flip appEndo)) . runWriterT

flatExp :: (Monad m, Eq a) => Exp a -> WriterT (Endo (Exp a)) m (Exp a)
flatExp (Let ds e) = do
  tell . Endo . Let =<< traverseOf (traversed . object . appObj) (runFlat . flatExp) ds
  flatExp e
flatExp (Match v [Unpack con ps e]) = do
  v <- flatExp v
  tell $ Endo $ \k -> Match v [Unpack con ps k]
  flatExp e
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
