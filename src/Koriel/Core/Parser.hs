{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Koriel.Core.Parser where

import Data.HashMap.Strict qualified as HashMap
import Error.Diagnose.Compat.Megaparsec
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Koriel.Core.Op
import Koriel.Core.Syntax hiding (atom, object)
import Koriel.Core.Type
import Koriel.Prelude hiding (many, some)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

parse :: String -> Text -> Either (ParseErrorBundle Text Void) (Program Text)
parse = Megaparsec.parse do
  space
  program

type Parser = Parsec Void Text

instance HasHints Void Text where
  hints = const []

-- | Parse a program.
program :: Parser (Program Text)
program = undefined

-- | Parse an unboxed literal.
unboxed :: Parser Unboxed
unboxed = try int32 <|> try int64 <|> try float <|> double <|> char <|> string <|> bool
  where
    int32 = lexeme do
      xs <- Lexer.decimal
      void $ Char.string "_i32"
      pure $ Int32 xs
    int64 = lexeme do
      xs <- Lexer.decimal
      void $ Char.string "_i64"
      pure $ Int64 xs
    float = lexeme do
      xs <- castWord32ToFloat <$> Lexer.hexadecimal
      void $ Char.string "_f32"
      pure $ Float xs
    double = lexeme do
      xs <- castWord64ToDouble <$> Lexer.hexadecimal
      void $ Char.string "_f64"
      pure $ Double xs
    char = lexeme do
      Char <$> between (Char.char '\'') (Char.char '\'') Lexer.charLiteral
    string = lexeme do
      String . toText <$> (Char.char '"' *> Lexer.charLiteral `manyTill` Char.char '"')
    bool = lexeme do
      xs <- Char.string "True#" <|> Char.string "False#"
      pure $ Bool $ xs == "True#"

-- | Parse an atom.
atom :: Parser (Atom Text)
atom = try (Var <$> ident) <|> Unboxed <$> unboxed

-- | Parse an object.
object :: Parser (Obj Text)
object = between (symbol "(") (symbol ")") do
  fun <|> pack <|> record
  where
    fun = do
      void $ symbol "fun"
      xs <- between (symbol "(") (symbol ")") (many ident)
      Fun xs <$> expr
    pack = do
      void $ symbol "pack"
      ty <- type_
      con <- constructor
      as <- many atom
      pure $ Pack ty con as
    record = do
      void $ symbol "record"
      kvs <-
        between
          (symbol "(")
          (symbol ")")
          ( many do
              k <- ident
              v <- between (symbol "(") (symbol ")") $ (,) <$> atom <*> type_
              pure (k, v)
          )
      pure $ Record $ HashMap.fromList kvs

-- | Parse an expression.
expr :: Parser (Exp Text)
expr =
  do
    Atom <$> atom
    <|> between (symbol "(") (symbol ")") do
      asum
        [ do
            void $ symbol "call"
            Call <$> atom <*> many atom,
          do
            void $ symbol "direct"
            CallDirect <$> ident <*> many atom,
          do
            void $ symbol "raw"
            RawCall <$> ident <*> type_ <*> many atom,
          do
            void $ symbol "binop"
            BinOp <$> operator <*> atom <*> atom,
          do
            void $ symbol "cast"
            Cast <$> type_ <*> atom,
          do
            void $ symbol "let"
            Let <$> between (symbol "(") (symbol ")") (many localDef) <*> expr,
          do
            void $ symbol "match"
            Match <$> expr <*> many case_
        ]

-- | Parse a local definition.
localDef :: Parser (LocalDef Text)
localDef = between (symbol "(") (symbol ")") do
  LocalDef <$> ident <*> type_ <*> object

case_ :: Parser (Case Text)
case_ = between (symbol "(") (symbol ")") do
  asum
    [ do
        void $ symbol "unpack"
        (c, xs) <- between (symbol "(") (symbol ")") do
          c <- constructor
          xs <- many ident
          pure (c, xs)
        Unpack c xs <$> expr,
      do
        void $ symbol "open"
        kvs <- between (symbol "(") (symbol ")") $ many do
          k <- ident
          v <- ident
          pure (k, v)
        OpenRecord (HashMap.fromList kvs) <$> expr,
      do
        void $ symbol "switch"
        Switch <$> unboxed <*> expr,
      do
        void $ symbol "bind"
        Bind <$> ident <*> type_ <*> expr
    ]

-- | Parse a type.
type_ :: Parser Type
type_ =
  between (symbol "(") (symbol ")") withParams
    <|> simple
  where
    simple =
      asumMap
        (\(n, t) -> try (symbol n) >> pure t)
        [ ("Int32#", Int32T),
          ("Int64#", Int64T),
          ("Float#", FloatT),
          ("Double#", DoubleT),
          ("Char#", CharT),
          ("String#", StringT),
          ("Bool#", BoolT),
          ("Any#", AnyT),
          ("Void#", VoidT)
        ]
    withParams =
      asum
        [ do
            void $ symbol "->"
            ps <- between (symbol "[") (symbol "]") (many type_)
            r <- type_
            pure $ ps :-> r,
          do
            void $ symbol "sum"
            cs <- many constructor
            pure $ SumT cs,
          do
            void $ symbol "Ptr#"
            PtrT <$> type_,
          do
            void $ symbol "Record#"
            fs <- many $ between (symbol "(") (symbol ")") do
              k <- ident
              v <- type_
              pure (k, v)
            pure $ RecordT $ HashMap.fromList fs
        ]

-- | Parse a constructor.
constructor :: Parser Con
constructor = between (symbol "(") (symbol ")") do
  tag <- tuple <|> data_
  args <- many type_
  pure $ Con tag args
  where
    tuple = void (symbol "Tuple#") >> pure Tuple
    data_ = Data <$> ident

-- | Parser an operator.
operator :: Parser Op
operator =
  asum
    [ try (symbol "+.") >> pure FAdd,
      try (symbol "-.") >> pure FSub,
      try (symbol "*.") >> pure FMul,
      try (symbol "/.") >> pure FDiv,
      symbol "+" >> pure Add,
      symbol "-" >> pure Sub,
      symbol "*" >> pure Mul,
      symbol "/" >> pure Div,
      symbol "%" >> pure Mod,
      symbol "==" >> pure Eq,
      try (symbol "<>") >> pure Neq,
      try (symbol "<=") >> pure Le,
      try (symbol ">=") >> pure Ge,
      symbol "<" >> pure Lt,
      symbol ">" >> pure Gt,
      symbol "&&" >> pure And,
      symbol "||" >> pure Or
    ]

-- * Common combinators

-- | Skip whitespace and comments.
space :: Parser ()
space = Lexer.space Char.space1 lineComment blockComment
  where
    lineComment = Lexer.skipLineComment ";"
    blockComment = Lexer.skipBlockCommentNested "#|" "|#"

-- | Apply a parser and skip trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

-- | Parse a symbol and skip trailing whitespace.
symbol :: Text -> Parser Text
symbol = Lexer.symbol space

-- | Character that can be used in an identifier.
-- Basically, it is the same as 'Malgo.Parser.identLetter', but we also allow '$' for temporary variables.
identLetter :: Parser Char
identLetter = Char.alphaNumChar <|> oneOf ("_+-*/\\%=><:;|&!#." :: String)

-- | Parse an identifier.
-- In Koriel, we always know where an identifier appears,
-- so we don't need to check if it is a keyword.
-- (And identifiers that textually look like keywords are allowed.)
ident :: Parser Text
ident = lexeme do
  xs <- some identLetter
  pure $ toText xs