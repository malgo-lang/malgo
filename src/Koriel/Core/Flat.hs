module Koriel.Core.Flat
  ( flat,
  )
where

import Control.Lens (traverseOf, traversed)
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
flatExp (Switch v cs) = pure $ Switch v (map (second flat) cs)
flatExp (Destruct v con ps e) = pure $ Destruct v con ps (flat e)
flatExp (Assign x v e) = do
  v <- flatExp v
  case v of
    Atom (Var v') -> flatExp $ replaceOf atom (Var x) (Var v') e
    _ -> do
      tell $ Endo $ \k -> Assign x v k
      flatExp e
flatExp e = pure e
