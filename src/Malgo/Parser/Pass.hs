module Malgo.Parser.Pass (ParserPass (..)) where

import Data.Text.Lazy qualified as TL
import Effectful (IOE, (:>))
import Effectful.Error.Static (throwError)
import Malgo.Module (Workspace)
import Malgo.Parser (parse)
import Malgo.Pass
import Malgo.Prelude hiding (throwError)
import Malgo.Syntax (Module)
import Malgo.Syntax.Extension (Malgo, MalgoPhase (NewParse))
import Text.Megaparsec (ParseErrorBundle)

data ParserPass = ParserPass

instance Pass ParserPass where
  type Input ParserPass = (FilePath, TL.Text)
  type Output ParserPass = ([Text], Module (Malgo NewParse))
  type ErrorType ParserPass = ParseErrorBundle TL.Text Void
  type Effects ParserPass es = (IOE :> es, Workspace :> es)

  runPassImpl _ (srcPath, text) = do
    result <- parse srcPath text
    case result of
      Left err -> throwError err
      Right result -> pure result
