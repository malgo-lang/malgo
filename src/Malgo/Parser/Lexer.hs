module Malgo.Parser.Lexer (Symbol (..), ReservedId (..), ReservedOp (..), WithPos (..), LexStream (..), lex, foldIndent) where

import Data.Text qualified as T
import Koriel.Pretty
import Malgo.Parser.Stream
import Malgo.Prelude hiding (Space, lex)
import Prettyprinter.Render.Text (renderStrict)
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (charLiteral, decimal, float, skipBlockComment, skipLineComment)

-- * Lexer

type Lexer = Parsec Void Text

lex :: String -> Text -> Either (ParseErrorBundle Text Void) LexStream
lex filename input =
  parse
    ( do
        symbols <- lexer <* eof
        pure LexStream {input = input, unLexStream = symbols}
    )
    filename
    input

lexer :: Lexer [WithPos Symbol]
lexer = do
  void $ many skipComment
  many do
    t <- lexSpace <|> lexSymbol <|> lexNewlines
    void $ many skipComment
    pure t
{-# INLINE lexer #-}

skipComment :: Lexer ()
skipComment = skipLineComment "--" <|> skipBlockComment "{-" "-}"

lexSpace :: Lexer (WithPos Symbol)
lexSpace = withPos do
  Space . sum <$> some (space <|> tab)
  where
    space = 1 <$ char ' '
    tab = tabLength <$ char '\t'
{-# INLINE lexSpace #-}

lexSymbol :: Lexer (WithPos Symbol)
lexSymbol = withPos do
  lexReservedId
    <|> lexReservedOp
    <|> lexParen
    <|> try lexQualified
    <|> lexIdent
    <|> lexOperator
    <|> lexInt
    <|> lexFloat
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
lexInt = fmap (Int False) decimal

lexFloat :: Lexer Symbol
lexFloat = fmap (Float False) float

lexChar :: Lexer Symbol
lexChar = label "char" do
  Char False <$> between (char '\'') (char '\'') charLiteral

lexString :: Lexer Symbol
lexString = label "string" do
  void $ char '"'
  String False . T.pack <$> manyTill charLiteral (char '"')

lexNewlines :: Lexer (WithPos Symbol)
lexNewlines = withPos do
  Newlines <$ takeWhile1P (Just "newlines") isNewline
  where
    isNewline c = c == '\r' || c == '\n'
{-# INLINE lexNewlines #-}

withPos :: Lexer Symbol -> Lexer (WithPos Symbol)
withPos m = do
  startPos <- getSourcePos
  startOffset <- getOffset
  value <- m
  endOffset <- getOffset
  endPos <- getSourcePos
  pure WithPos {startPos, endPos, length = endOffset - startOffset + 1, value}
{-# INLINEABLE withPos #-}

-- * Recognize Indentation

-- | Convert every pair of @Newlines@ and @Space@ to @IndentStart@ and @IndentEnd@.
--
-- Rules:
--
-- - newline and m spaces -> @IndentStart m@ (current indentation level n \< m) or @IndentEnd m@ (current indentation level n \> m)
-- - If current indentation level n == m, insert @IndentEnd m@  and @IndentStart m@
foldIndent :: LexStream -> LexStream
foldIndent l@LexStream {unLexStream} = l {unLexStream = go [] mempty $ preprocess unLexStream}
  where
    -- remove Space between Newlines (empty lines)
    preprocess :: [WithPos Symbol] -> [WithPos Symbol]
    preprocess [] = []
    preprocess (WithPos {value = Newlines} : WithPos {value = Space _} : x@WithPos {value = Newlines} : xs) = x : preprocess xs
    preprocess (x : xs) = x : preprocess xs
    go :: [Int] -> [WithPos Symbol] -> [WithPos Symbol] -> [WithPos Symbol]
    go [] acc [] = reverse acc
    go ns acc@(WithPos {endPos} : _) [] = go [] acc $ map (\l -> WithPos {startPos = endPos, endPos, length = 0, value = IndentEnd l}) ns
    go (_ : _) [] [] = error "impossible"
    go ns acc (WithPos {startPos, endPos = endPosN, length = l1, value = Newlines} : WithPos {startPos = startPosS, endPos, length = l2, value = Space m} : xs') =
      case ns of
        []
          | m > 0 ->
              -- Insert IndentStart and go to level m
              go [m] (WithPos {startPos, endPos, length = l1 + l2, value = IndentStart m} : acc) xs'
          | otherwise ->
              -- If m == 0, just ignore Newlines and Space
              go [] acc xs'
        n : ns
          | m > n ->
              -- Insert IndentStart and go to level m
              go (m : n : ns) (WithPos {startPos, endPos, length = l1 + l2, value = IndentStart m} : acc) xs'
          | m < n ->
              -- Insert IndentEnd and go to level m
              let (dropped, rest) = span (m <) (n : ns)
               in go rest (map (\l -> WithPos {startPos, endPos, length = 0, value = IndentEnd l}) (reverse dropped) <> acc) xs'
          | otherwise ->
              -- Continue current level
              go
                (n : ns)
                ( WithPos {startPos = startPosS, endPos, length = l2, value = IndentStart n}
                    : WithPos {startPos, endPos = endPosN, length = l1, value = IndentEnd n}
                    : acc
                )
                xs'
    go ns acc (x@WithPos {endPos, value = Newlines} : xs') =
      -- insert Space 0 to xs
      go ns acc (x : WithPos {startPos = endPos, endPos, length = 0, value = Space 0} : xs')
    go ns acc (WithPos {value = Space _} : xs) = go ns acc xs
    go ns acc (x : xs) = go ns (x : acc) xs
