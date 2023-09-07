{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Parser (parse) where

import Data.Functor (void)
import Data.Functor.Identity (Identity)
import Data.String.Conversions (ConvertibleStrings (convertString))
import Data.Text (Text)
import Data.Text.ICU.Char (Bool_ (XidContinue, XidStart), property)
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
import Text.Parsec.Token (GenLanguageDef (..), GenTokenParser, makeTokenParser)
import Text.Parsec.Token qualified as Token

parse :: FilePath -> Text -> Either Text (Expr Text)
parse filePath input =
  case Parsec.parse program filePath input of
    Left err ->
      let fromErr :: Diagnostic Text = errorDiagnosticFromParseError Nothing "Parse error" Nothing err
          withFile = addFile fromErr filePath (convertString input)
          pretty = prettyDiagnostic WithUnicode (TabSize 4) withFile
          msg = renderStrict $ layoutSmart defaultLayoutOptions pretty
       in Left msg
    Right result -> Right result

instance HasHints Void Text where
  hints _ = mempty

type Parser a = Parsec Text () a

program :: Parser (Expr Text)
program = expr

expr :: Parser (Expr Text)
expr = apply

apply :: Parser (Expr Text)
apply = do
  f <- atom
  args <- many atom
  case args of
    [] -> pure f
    a : as -> pure $ App f (a : as)

atom :: Parser (Expr Text)
atom = choice [var, lit, parens expr, codata]

var :: Parser (Expr Text)
var = Var <$> getPosition <*> identifier

lit :: Parser (Expr Text)
lit = Lit <$> getPosition <*> (Int <$> try integer)

codata :: Parser (Expr Text)
codata = braces $ Codata <$> getPosition <*> (clause `sepEndBy1` colon)

clause :: Parser (Clause Text)
clause = Clause <$> pat <*> (reservedOp "->" *> expr)

pat :: Parser (Pat Text)
pat = do
  mpat <- exprToPat <$> expr
  case mpat of
    Just result -> pure result
    Nothing -> fail "not a pattern"

-- | @identifier = (XID_Start | _ | #) (XID_Continue | _ | #)*@
identifier :: Parser Text
identifier = convertString <$> Token.identifier lexer

integer :: Parser Integer
integer = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

colon :: Parser ()
colon = void $ Token.colon lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

lexer :: GenTokenParser Text () Identity
lexer = makeTokenParser language

language :: GenLanguageDef Text () Identity
language =
  LanguageDef
    { commentStart = "{-",
      commentEnd = "-}",
      commentLine = "--",
      nestedComments = True,
      identStart = satisfy $ \c -> property XidStart c || c == '_' || c == '#',
      identLetter = satisfy $ \c -> property XidContinue c || c == '_' || c == '#',
      opStart = language.opLetter,
      opLetter = satisfy $ \c -> elem @[] c ":!$%&*+./<=>?@\\^|-~",
      reservedNames = [],
      reservedOpNames = ["->"],
      caseSensitive = True
    }