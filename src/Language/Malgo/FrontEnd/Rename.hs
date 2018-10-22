module Language.Malgo.FrontEnd.Rename where

import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Language.Malgo.Type
import           Universum                       hiding (Type)

rename :: RnTcEnv -> [Decl Text] -> MalgoM [Decl Id]
rename = undefined
