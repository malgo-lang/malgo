module Malgo.Parser.Lexer (Symbol (..), ReservedId (..), ReservedOp (..), WithPos (..), LexStream (..), lex) where

import Control.Monad.State
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust)
import Data.Text qualified as T
import GHC.Float (double2Float)
import Koriel.Pretty
import Malgo.Parser.Stream
import Malgo.Prelude hiding (Space, lex)
import Prettyprinter.Render.Text (renderStrict)
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (charLiteral, decimal, float, skipBlockComment, skipLineComment)

-- * Lexer

type Lexer = StateT (NonEmpty Int) (Parsec Void Text)

lex :: String -> Text -> Either (ParseErrorBundle Text Void) LexStream
lex filename input =
  parse
    ( do
        symbols <- evalStateT lexer (NonEmpty.fromList [0]) <* eof
        pure LexStream {input = input, unLexStream = symbols}
    )
    filename
    input

lexer :: Lexer [WithPos Symbol]
lexer = do
  void $ many skipComment
  ts <- many do
    t <- pure <$> lexSymbol <|> lexIndent <|> pure <$> lexSpace <|> pure <$> lexNewlines
    void $ many skipComment
    pure t
  pos <- getSourcePos
  ends <- gets (fmap (at pos pos 0 . IndentEnd))
  pure $ join ts <> init (NonEmpty.toList ends)
{-# INLINE lexer #-}

skipComment :: Lexer ()
skipComment = skipLineComment "--" <|> skipBlockComment "{-" "-}"

indentLevel :: Lexer Int
indentLevel = do
  ns <- get
  case ns of
    n :| _ -> pure n

lexIndent :: Lexer [WithPos Symbol]
lexIndent = try do
  n <- indentLevel
  startPos <- getSourcePos
  startOffset <- getOffset
  void $ takeWhile1P (Just "newlines") isNewline
  count <- sum <$> many (space <|> tab)
  notFollowedBy (satisfy isNewline) -- skip empty line
  endOffset <- getOffset
  endPos <- getSourcePos
  if
    | count > n -> do
        modify (NonEmpty.cons count)
        pure [at startPos endPos (endOffset - startOffset) $ IndentStart count]
    | count < n -> do
        ns <- get
        let (dropped, rest) = span (count <) (NonEmpty.toList ns)
        modify (const $ NonEmpty.fromList rest)
        case dropped of
          [] -> error "unreachable"
          x : xs -> pure $ at startPos endPos (endOffset - startOffset) (IndentEnd x) : map (at endPos endPos 0 . IndentEnd) xs
    | n == 0 -> empty
    | otherwise -> pure [at startPos endPos (endOffset - startOffset) $ IndentEnd n, at endPos endPos 0 (IndentStart n)]
  where
    space = 1 <$ char ' '
    tab = tabLength <$ char '\t'
    isNewline c = c == '\r' || c == '\n'

at :: SourcePos -> SourcePos -> Int -> a -> WithPos a
at startPos endPos length x = WithPos {startPos, endPos, length, value = x}

lexSpace :: Lexer (WithPos Symbol)
lexSpace = withPos do
  Space . sum <$> some (space <|> tab)
  where
    space = 1 <$ char ' '
    tab = tabLength <$ char '\t'
{-# INLINE lexSpace #-}

lexNewlines :: Lexer (WithPos Symbol)
lexNewlines = withPos do
  Newlines <$ takeWhile1P (Just "newlines") isNewline
  where
    isNewline c = c == '\r' || c == '\n'
{-# INLINE lexNewlines #-}

lexSymbol :: Lexer (WithPos Symbol)
lexSymbol = withPos do
  lexReservedId
    <|> lexReservedOp
    <|> lexParen
    <|> try lexQualified
    <|> lexIdent
    <|> lexOperator
    <|> try lexFloat
    <|> lexInt
    <|> lexChar
    <|> lexString

lexReservedId :: Lexer Symbol
lexReservedId = label "reserved identifier" $ try do
  choice (map (\(s, t) -> try $ string s >> notFollowedBy (satisfy isIdentStart) >> pure (ReservedId t)) reservedIdTable)
{-# INLINE lexReservedId #-}

lexReservedOp :: Lexer Symbol
lexReservedOp = label "reserved operator" $ try do
  choice (map (\(s, t) -> try $ string s >> notFollowedBy (satisfy isOperator) >> pure (ReservedOp t)) reservedOpTable)
{-# INLINE lexReservedOp #-}

lexParen :: Lexer Symbol
lexParen = label "brackets" do
  choice (map (\(s, t) -> ReservedOp t <$ char s) parenTable)

reservedIdTable :: [(Text, ReservedId)]
reservedIdTable =
  map (\t -> (renderStrict $ layoutCompact $ pretty t, t)) [minBound .. maxBound]
{-# INLINE reservedIdTable #-}

reservedOpTable :: [(Text, ReservedOp)]
reservedOpTable =
  map (\t -> (renderStrict $ layoutCompact $ pretty t, t)) [DArrow .. DotDot]
{-# INLINE reservedOpTable #-}

parenTable :: [(Char, ReservedOp)]
parenTable = [('{', LBrace), ('}', RBrace), ('(', LParen), (')', RParen), ('[', LBracket), (']', RBracket)]
{-# INLINE parenTable #-}

lexQualified :: Lexer Symbol
lexQualified = label "qualified identifier" do
  mc <- satisfy isIdentStart
  mcs <- takeWhileP Nothing isIdent
  void $ char '.'
  c <- lexIdent <|> lexOperator
  pure $ Qualified (T.cons mc mcs) c

lexIdent :: Lexer Symbol
lexIdent = label "identifier" do
  c <- satisfy isIdentStart
  cs <- takeWhileP Nothing isIdent
  pure (Ident $ T.cons c cs)
{-# INLINE lexIdent #-}

isIdentStart :: Char -> Bool
isIdentStart c = isLetter c || c == '_'

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_' || c == '#'

lexOperator :: Lexer Symbol
lexOperator = label "operator" do
  Operator <$> takeWhile1P Nothing isOperator

isOperator :: Char -> Bool
isOperator c = c `elem` ("+-*/\\%=><:;|&!#." :: String)

lexInt :: Lexer Symbol
lexInt = do
  x <- decimal
  isInt64 <- isJust <$> optional (char 'l' <|> char 'L')
  unboxed <- isJust <$> optional (char '#')
  pure $ if isInt64 then Int64 unboxed (fromInteger x) else Int32 unboxed (fromInteger x)

lexFloat :: Lexer Symbol
lexFloat = do
  x <- float
  isFloat <- isJust <$> optional (char 'f' <|> char 'F')
  unboxed <- isJust <$> optional (char '#')
  pure $ if isFloat then Float unboxed (double2Float x) else Double unboxed x

lexChar :: Lexer Symbol
lexChar = label "char" do
  x <- between (char '\'') (char '\'') charLiteral
  unboxed <- isJust <$> optional (char '#')
  pure $ Char unboxed x

lexString :: Lexer Symbol
lexString = label "string" do
  void $ char '"'
  x <- T.pack <$> manyTill charLiteral (char '"')
  unboxed <- isJust <$> optional (char '#')
  pure $ String unboxed x

withPos :: Lexer Symbol -> Lexer (WithPos Symbol)
withPos m = do
  startPos <- getSourcePos
  startOffset <- getOffset
  value <- m
  endOffset <- getOffset
  endPos <- getSourcePos
  pure WithPos {startPos, endPos, length = endOffset - startOffset, value}
{-# INLINEABLE withPos #-}
