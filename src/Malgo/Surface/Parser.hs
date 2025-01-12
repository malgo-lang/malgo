{-# LANGUAGE TemplateHaskell #-}

module Malgo.Surface.Parser (parse) where

import Control.Arrow ((>>>))
import Control.Monad (void, when)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (isAlphaNum)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Void (Void)
import Malgo.Location (Location (..))
import Malgo.Prelude
import Malgo.Surface
import Text.Megaparsec hiding (Label, parse)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

parse :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Definition Text]
parse = runParser do
  defs <- many pDefinition
  _ <- eof
  pure defs

type Parser = Parsec Void Text

getLocation :: Parser Location
getLocation = do
  sourcePos <- getSourcePos
  pure
    $ Location
      { fileName = sourceName sourcePos,
        line = unPos $ sourceLine sourcePos,
        column = unPos $ sourceColumn sourcePos
      }

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pKeyword :: Text -> Parser Text
pKeyword str = lexeme $ string str <* notFollowedBy alphaNumChar

reservedKeywords :: [Text]
reservedKeywords = ["def", "match", "switch", "default", "label", "goto"]

unexpectedToken :: [Text] -> Text -> Parser a
unexpectedToken expected name
  | name `elem` reservedKeywords = failure (Just $ textToTokens name) (Set.fromList $ map textToLabel expected)
  | otherwise = error "unexpectedReservedKeyword: name is not a reserved keyword"

textToTokens :: Text -> ErrorItem Char
textToTokens = Tokens . NE.fromList . convertString

textToLabel :: Text -> ErrorItem Char
textToLabel = Megaparsec.Label . NE.fromList . convertString

pIdentifier :: Parser Text
pIdentifier = label "identifier" $ lexeme do
  first <- letterChar
  rest <- takeWhileP (Just "alpha num character") isAlphaNum
  let name = T.cons first rest
  when (name `elem` reservedKeywords)
    $ unexpectedToken ["identifier"] name
  pure name

pDefinition :: Parser (Definition Text)
pDefinition = label "definition" do
  location <- getLocation
  _ <- pKeyword "def"
  name <- pIdentifier
  (params, returns) <- pArgumentList pIdentifier
  _ <- symbol "="
  term <- pTerm
  pure $ Definition {..}

pArgumentList :: Parser a -> Parser ([a], [a])
pArgumentList pItem = between (symbol "(") (symbol ")") $ do
  params <- pItem `sepBy` symbol ","
  maybeSeparator <- optional $ symbol ";"
  case maybeSeparator of
    Just _ -> do
      returns <- pItem `sepBy` symbol ","
      pure (params, returns)
    Nothing -> pure (params, [])

pTerm :: Parser (Term Text)
pTerm =
  makeExprParser pAtomicTerm table
  where
    table = [[postfixOperator], [labelOperator]]

-- By default, makeExprParser does not allow chaining of postfix operators.
-- To allow chaining, we use the following strategy:
-- 1. Read postfix operators multiple times while they are available.
-- 2. Fold the list of postfix operators into a single function using foldr1 (>>>).
-- 3. Pass it to makeExprParser as a single postfix operator.
makeChainable :: Parser (a -> a) -> Parser (a -> a)
makeChainable p = foldr1 (>>>) <$> some p

postfixOperator :: Operator Parser (Term Text)
postfixOperator =
  Postfix
    $ makeChainable
    $ choice
      [ applyOperator,
        destructOperator,
        matchOperator,
        switchOperator,
        gotoOperator
      ]

applyOperator :: Parser (Term Text -> Term Text)
applyOperator = label "apply" $ makeChainable do
  location <- getLocation
  (producers, consumers) <- pArgumentList pTerm
  pure \term -> Apply {..}

destructOperator :: Parser (Term Text -> Term Text)
destructOperator =
  label "destruct" $ makeChainable do
    location <- getLocation
    _ <- symbol "."
    tag <- pIdentifier
    (producers, consumers) <- pArgumentList pTerm
    pure \term -> Destruct {..}

matchOperator :: Parser (Term Text -> Term Text)
matchOperator = label "match" $ makeChainable do
  location <- getLocation
  _ <- symbol "match"
  clauses <- between (symbol "{") (symbol "}") $ pClause `sepEndBy` symbol ","
  pure \term -> Match {..}

switchOperator :: Parser (Term Text -> Term Text)
switchOperator = label "switch" $ makeChainable do
  location <- getLocation
  _ <- symbol "switch"
  (branches, defaultBranch) <- between (symbol "{") (symbol "}") do
    branches <- pBranch `endBy` symbol ","
    defaultBranch <- pKeyword "default" *> symbol "->" *> pTerm
    _ <- optional $ symbol ","
    pure (branches, defaultBranch)
  pure \term -> Switch {..}

gotoOperator :: Parser (Term Text -> Term Text)
gotoOperator = label "goto" do
  location <- getLocation
  _ <- symbol "goto"
  name <- pIdentifier
  pure \term -> Goto {..}

labelOperator :: Operator Parser (Term Text)
labelOperator = Prefix $ label "label" do
  location <- getLocation
  _ <- symbol "label"
  name <- pIdentifier
  pure \term -> Label {..}

pBranch :: Parser (Literal, Term Text)
pBranch = label "switch branch" do
  literal <- pLiteral'
  _ <- symbol "->"
  term <- pTerm
  pure (literal, term)

pClause :: Parser (Clause Text)
pClause = label "pattern clause" do
  pattern <- pPattern
  _ <- symbol "->"
  term <- pTerm
  pure Clause {..}

pPattern :: Parser (Pattern Text)
pPattern = label "pattern" do
  tag <- pIdentifier
  (params, returns) <- pArgumentList pIdentifier
  pure Pattern {..}

pAtomicTerm :: Parser (Term Text)
pAtomicTerm =
  label "atomic term"
    $ choice
      [ pVar,
        pLiteral,
        pComatch
      ]

pVar :: Parser (Term Text)
pVar = label "var" do
  location <- getLocation
  name <- pIdentifier
  pure $ Var {..}

pLiteral :: Parser (Term Text)
pLiteral = label "literal" do
  location <- getLocation
  literal <- pLiteral'
  pure $ Literal {..}

pLiteral' :: Parser Literal
pLiteral' = label "literal (inner)" $ lexeme $ Int <$> L.decimal

-- | Parse a comatch term.
-- @ {coclauses...} @
--
-- INFO: If you want to add a new term starting with a brace, you need to modify this function.
pComatch :: Parser (Term Text)
pComatch = label "comatch" do
  location <- getLocation
  coclauses <- between (symbol "{") (symbol "}") $ pCoclause `sepEndBy` symbol ","
  pure $ Comatch {..}

-- | Parse a coclause.
-- @ $tag(params...;returns...) -> term @
pCoclause :: Parser (Coclause Text)
pCoclause = label "comatch clause" do
  copattern <- pCopattern
  _ <- symbol "->"
  term <- pTerm
  pure Coclause {..}

-- | Parse a copattern.
-- @ tag(params...;returns...) @
pCopattern :: Parser (Copattern Text)
pCopattern = label "copattern" do
  void $ symbol "."
  tag <- pIdentifier
  (params, returns) <- pArgumentList pIdentifier
  pure Copattern {..}