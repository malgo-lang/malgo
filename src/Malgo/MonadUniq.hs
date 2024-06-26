module Malgo.MonadUniq (getUniq, Uniq (..)) where

import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, state)
import Malgo.Prelude

newtype Uniq = Uniq Int

getUniq :: (State Uniq :> es) => Eff es Int
getUniq = state $ \(Uniq u) -> (u, Uniq $ u + 1)
