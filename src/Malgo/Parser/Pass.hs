module Malgo.Parser.Pass (ParserPass (..), parse, Parser) where

import Data.Text.Lazy qualified as TL
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (throwError)
import Effectful.FileSystem (runFileSystem)
import Malgo.Features (Features, parseFeatures, addFeatures)
import Malgo.Module (Workspace)
import Malgo.Parser.Common
import Malgo.Parser.Declaration
import Malgo.Parser.Lexer (extractPragmas, space)
import Malgo.Pass
import Malgo.Prelude
import Malgo.Syntax (Module)
import Malgo.Syntax.Extension (Malgo, MalgoPhase (Parse))
import Text.Megaparsec (ParseErrorBundle, runParserT, eof)

data ParserPass = ParserPass

instance Pass ParserPass where
  type Input ParserPass = (FilePath, TL.Text)
  type Output ParserPass = Module (Malgo Parse)
  type ErrorType ParserPass = ParseErrorBundle TL.Text Void
  type Effects ParserPass es = (IOE :> es, Workspace :> es, Features :> es)

  runPassImpl _ (srcPath, text) = do
    result <- parse srcPath text
    case result of
      Left err -> throwError err
      Right result -> pure result

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

