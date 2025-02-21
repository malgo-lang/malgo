module Malgo.Sequent.Value (Value (..), Env (..)) where

import Malgo.Prelude
import Malgo.Sequent.Command (Command)
import Malgo.Sequent.Fun (Literal, Name, Tag)

data Value where
  Literal :: Literal -> Value
  Struct :: Tag -> [Value] -> Value
  Function :: Env -> [Name] -> Command -> Value
  Object :: Env -> Map Text Command -> Value

data Env = Env
  { parent :: Maybe Env,
    bindings :: Map Name Value
  }
