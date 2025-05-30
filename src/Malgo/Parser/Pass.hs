module Malgo.Parser.Pass (ParserPass (..)) where

import Data.Text.Lazy qualified as TL
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, throwError)
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
  type Effects ParserPass es = (IOE :> es, Workspace :> es, Error (ParseErrorBundle TL.Text Void) :> es)

  runPass :: (Effects ParserPass es) => ParserPass -> Input ParserPass -> Eff es (Output ParserPass)
  runPass _ (srcPath, text) = do
    result <- parse srcPath text
    case result of
      Left err -> throwError err
      Right result -> pure result