{-# LANGUAGE TypeFamilies #-}

module Malgo.Unique
  ( UniqueGen,
    newUnique,
    runUniqueGen,
  )
where

import Effectful.State.Static.Shared qualified as Shared
import Malgo.Prelude

-- | @UniqueGen@ is an effect for generating unique numbers
type UniqueGen = Shared.State Int

runUniqueGen :: Eff (UniqueGen : es) a -> Eff es a
runUniqueGen = Shared.evalState 0

-- | @newUnique@ generates a new unique number
newUnique :: (UniqueGen :> es) => Eff es Int
newUnique = do
  i <- Shared.get
  Shared.put (i + 1)
  pure i
