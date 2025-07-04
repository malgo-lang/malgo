module Malgo.Parser
  ( parse,
    -- * Re-exports for backwards compatibility
    Parser,
  )
where

import Data.Text.Lazy qualified as TL
import Effectful
import Effectful.FileSystem (runFileSystem)
import Malgo.Features
import Malgo.Module (Workspace)
import Malgo.Parser.Common
import Malgo.Parser.Declaration
import Malgo.Parser.Lexer
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec hiding (parse)

-- | parse a module. Returns the list of pragmas and the module.
parse :: (IOE :> es, Workspace :> es, Features :> es) => FilePath -> TL.Text -> Eff es (Either (ParseErrorBundle TL.Text Void) (Module (Malgo Parse)))
parse srcPath text = runFileSystem do
  let features = parseFeatures $ extractPragmas text
  addFeatures features
  runParserT parser srcPath text

parser :: (IOE :> es, Workspace :> es, Features :> es) => Parser es (Module (Malgo Parse))
parser = do
  space
  mod <- pModuleFile
  eof
  pure mod

