module Malgo.Syntax.ToCore where

import Malgo.Core qualified as Core
import Malgo.Name
import Malgo.Prelude
import Malgo.Syntax

toCore :: [Definition Name] -> Eff es [Core.Definition]
toCore = undefined