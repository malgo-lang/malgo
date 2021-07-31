module Malgo.Refine.RefineEnv where

import Koriel.Id
import Malgo.Prelude
import Malgo.TypeCheck.TcEnv
import Malgo.TypeRep.Static

type RefineEnv = HashMap (Id Kind) (TypeDef Type)

buildRefineEnv :: TcEnv -> RefineEnv
buildRefineEnv TcEnv {_typeEnv} = undefined
