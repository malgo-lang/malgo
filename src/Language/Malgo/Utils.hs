module Language.Malgo.Utils where

import Text.PrettyPrint.HughesPJClass
import RIO.Text

instance Pretty Text where
  pPrint = pPrint . unpack
