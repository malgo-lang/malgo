{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Parser (parse) where

import Data.String.Conversions (ConvertibleStrings (convertString))
import Data.Text (Text)
import Data.Void (Void)
import Error.Diagnose (TabSize (TabSize), WithUnicode (WithUnicode), addFile, prettyDiagnostic)
import Error.Diagnose.Compat.Parsec (HasHints (hints), errorDiagnosticFromParseError)
import Error.Diagnose.Diagnostic (Diagnostic)
import Malgo.Prelude
import Malgo.Syntax
import Prettyprinter (defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.Text (renderStrict)
import Text.Parsec hiding (parse)
import Text.Parsec qualified as Parsec

parse :: FilePath -> Text -> Either Text (Expr Text)
parse filePath input =
  case Parsec.parse program filePath input of
    Left err ->
      let fromErr :: Diagnostic Text = errorDiagnosticFromParseError Nothing "Parse error" Nothing err
          withFile = addFile fromErr filePath (convertString input)
          pretty = prettyDiagnostic WithUnicode (TabSize 4) withFile
          msg = renderStrict $ layoutSmart defaultLayoutOptions pretty
       in Left msg
    Right expr -> Right expr

instance HasHints Void Text where
  hints _ = mempty

type Parser a = Parsec Text () a

program :: Parser (Expr Text)
program = pure $ Var "_"