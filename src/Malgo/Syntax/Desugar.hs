module Malgo.Syntax.Desugar (desugar, DesugarError) where

import Effectful.Error.Static (Error, throwError)
import Malgo.Prelude
import Malgo.Syntax

desugar :: (Error DesugarError :> es) => [Definition Raw a] -> Eff es [Definition Desugared a]
desugar = traverse convert

data DesugarError
  = Unimplemented
  deriving (Show)

class Convert a r where
  convert :: a -> r

instance (Error DesugarError :> es) => Convert (Definition Raw a) (Eff es (Definition Desugared a)) where
  convert _ = throwError Unimplemented