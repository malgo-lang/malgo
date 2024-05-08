module Malgo.Core.Mangle (mangleProgram) where

import Malgo.Core.Syntax
import Malgo.Id
import Malgo.Mangle
import Malgo.Module (moduleNameToString)
import Malgo.Prelude

mangleProgram :: Program (Meta a) -> Program Text
mangleProgram = fmap mangleMeta

mangleMeta :: Meta a -> Text
mangleMeta Meta {..} = mangleId id

mangleId :: Id -> Text
mangleId Id {..} = mangle [name, moduleNameToString moduleName, idSortToText sort]
  where
    idSortToText External = "External"
    idSortToText (Internal uniq) = "Internal" <> convertString (show uniq)
    idSortToText (Temporal uniq) = "Temporal" <> convertString (show uniq)
    idSortToText Native = "Native"