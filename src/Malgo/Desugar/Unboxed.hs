module Malgo.Desugar.Unboxed (dsUnboxed) where

-- Unboxedの脱糖衣

import Koriel.Core.Syntax as Core
import Malgo.Prelude
import Malgo.Syntax as Malgo
import Malgo.Syntax.Extension as Malgo

dsUnboxed :: Literal Malgo.Unboxed -> Core.Unboxed
dsUnboxed (Malgo.Int32 x) = Core.Int32 $ toInteger x
dsUnboxed (Malgo.Int64 x) = Core.Int64 $ toInteger x
dsUnboxed (Malgo.Float x) = Core.Float x
dsUnboxed (Malgo.Double x) = Core.Double x
dsUnboxed (Malgo.Char x) = Core.Char x
dsUnboxed (Malgo.String x) = Core.String x
