module Malgo.Machine.Value (Value (..), Env (..)) where

import Malgo.Machine.Command (Name, Tag)
import Malgo.Machine.Command qualified as C
import Malgo.Prelude

data Value where
  Literal :: C.Literal -> Value
  Struct :: Tag -> [Value] -> Value
  Function :: Env -> Name -> C.Command -> Value
  Object :: Env -> Map Text C.Command -> Value

data Env = Env
  { parent :: Maybe Env,
    bindings :: Map Name Value
  }