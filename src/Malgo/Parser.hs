{-# LANGUAGE TemplateHaskell #-}

module Malgo.Parser (parse) where

import Control.Monad (when)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (isAlphaNum)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Void
import Debug.Trace (traceM)
import Malgo.Location
import Malgo.Prelude
import Malgo.Syntax
import Text.Megaparsec hiding (Label, parse)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
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

withSigil :: Sigil -> Parser Text
withSigil sigil = do
  _ <- char $ sigilToChar sigil
  pIdentifier

data Sigil = Toplevel | Constructor | Destructor | Primitive

sigilToChar :: Sigil -> Char
sigilToChar Toplevel = '@'
sigilToChar Constructor = '$'
sigilToChar Primitive = '#'
sigilToChar Destructor = '.'

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
  name <- withSigil Toplevel
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
    table = [[destructOperator], [matchOperator, switchOperator], [gotoOperator], [labelOperator]]

destructOperator :: Operator Parser (Term Text)
destructOperator = Postfix $ label "destruct" do
  location <- getLocation
  _ <- symbol "."
  tag <- pIdentifier
  (producers, consumers) <- pArgumentList pTerm
  pure \term -> Destruct {..}

matchOperator :: Operator Parser (Term Text)
matchOperator = Postfix $ label "match" do
  location <- getLocation
  _ <- symbol "match"
  clauses <- between (symbol "{") (symbol "}") $ pClause `sepEndBy` symbol ","
  pure \term -> Match {..}

switchOperator :: Operator Parser (Term Text)
switchOperator = Postfix $ label "switch" do
  location <- getLocation
  _ <- symbol "switch"
  (branches, defaultBranch) <- between (symbol "{") (symbol "}") do
    traceM "pSwitch"
    branches <- pBranch `endBy` symbol ","
    traceM "pSwitch branches"
    defaultBranch <- pKeyword "default" *> symbol "->" *> pTerm
    _ <- optional $ symbol ","
    pure (branches, defaultBranch)
  pure \term -> Switch {..}

gotoOperator :: Operator Parser (Term Text)
gotoOperator = Postfix $ label "goto" do
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
  traceM "pBranch"
  literal <- pLiteral'
  traceM "pBranch literal"
  _ <- symbol "->"
  traceM "pBranch ->"
  term <- pTerm
  traceM "pBranch term"
  pure (literal, term)

pClause :: Parser (Clause Text)
pClause = label "pattern clause" do
  pattern <- pPattern
  _ <- symbol "->"
  term <- pTerm
  pure Clause {..}

pPattern :: Parser (Pattern Text)
pPattern = label "pattern" do
  tag <- withSigil Constructor
  (params, returns) <- pArgumentList pIdentifier
  pure Pattern {..}

pAtomicTerm :: Parser (Term Text)
pAtomicTerm =
  label "atomic term"
    $ choice
      [ pVar,
        pLiteral,
        pConstruct,
        pComatch,
        pPrim,
        pInvoke
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

-- | Parse a construct term.
-- @ $tag(producers...;consumers...) @
pConstruct :: Parser (Term Text)
pConstruct = label "construct" do
  location <- getLocation
  tag <- withSigil Constructor
  (producers, consumers) <- pArgumentList pTerm
  pure $ Construct {..}

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
  tag <- withSigil Destructor
  (params, returns) <- pArgumentList pIdentifier
  pure Copattern {..}

-- | Parse a primitive procedure call.
-- @ #tag(producers...;consumers...) @
pPrim :: Parser (Term Text)
pPrim = label "prim" do
  location <- getLocation
  tag <- withSigil Primitive
  (producers, consumers) <- pArgumentList pTerm
  pure $ Prim {..}

pInvoke :: Parser (Term Text)
pInvoke = label "invoke" do
  location <- getLocation
  name <- withSigil Toplevel
  (producers, consumers) <- pArgumentList pTerm
  pure $ Invoke {..}