module Koriel.Core.Flat
  ( flat,
  )
where

import Control.Lens (traverseOf, traversed)
import Koriel.Core.Syntax
import Koriel.Prelude

flat :: Exp a -> Exp a
flat e = runIdentity $ runFlat (flatExp e)

runFlat :: Monad m => WriterT (Endo r) m r -> m r
runFlat = fmap (uncurry (flip appEndo)) . runWriterT

flatExp :: Monad m => Exp a -> WriterT (Endo (Exp a)) m (Exp a)
flatExp (Let ds e) = do
  tell . Endo . Let =<< traverseOf (traversed . localDefObj . appObj) (runFlat . flatExp) ds
  flatExp e
flatExp (Match vs (Case pats e :| [])) = do
  vs' <- traverse flatExp vs
  tell $ Endo $ \k -> Match vs' (Case pats k :| [])
  flatExp e
flatExp (Match es cs) =
  Match <$> traverse flatExp es <*> traverseOf (traversed . appCase) (runFlat . flatExp) cs
flatExp e = pure e
