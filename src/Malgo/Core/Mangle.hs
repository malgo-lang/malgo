module Malgo.Core.Mangle (mangleProgram) where

import Malgo.Core.Syntax
import Malgo.Id
import Malgo.Mangle
import Malgo.Prelude

mangleProgram :: Program (Meta a) -> Program Text
mangleProgram = fmap mangleMeta

mangleMeta :: Meta a -> Text
mangleMeta Meta {..} = mangle id
