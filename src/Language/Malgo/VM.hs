module Language.Malgo.VM where

import qualified Language.Malgo.Syntax as Syntax

type Env = [(Syntax.Name, Data)]

data Data = Nil
          | Symbol Syntax.Name
          | Int Int
          | Float Float
          | Bool Bool
          | List [Data]

data SECD = SECD { s :: [Data], e :: Env}
