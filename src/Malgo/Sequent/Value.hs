module Malgo.Sequent.Value (Value (..), Env (..)) where

import Malgo.Prelude
import Malgo.Sequent.Command (Name, Tag)
import Malgo.Sequent.Command qualified as C

data Value where
  Literal :: C.Literal -> Value
  Struct :: Tag -> [Value] -> Value
  Function :: Env -> [Name] -> C.Command -> Value
  Object :: Env -> Map Text C.Command -> Value

data Env = Env
  { parent :: Maybe Env,
    bindings :: Map Name Value
  }
