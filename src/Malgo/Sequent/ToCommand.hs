module Malgo.Sequent.ToCommand (toCommand) where

import Effectful
import Effectful.State.Static.Local (State)
import Malgo.Id
import Malgo.Prelude
import Malgo.Sequent.Command qualified as C
import Malgo.Sequent.Core
import Malgo.Sequent.Fun (Literal, Name, Pattern, Tag)
import Malgo.MonadUniq
import Malgo.Sequent.Command (Command)

toCommand :: (State Uniq :> es) => Program Zero -> Eff es C.Program
toCommand (Program definitions) = C.Program <$> traverse convert definitions

class Convert a b where
  convert :: a -> b

instance (State Uniq :> es) => Convert (Range, Name, [Name], Statement Zero) (Eff es (Range, Name, [Command])) where
  convert (range, name, params, body) = do
    undefined



