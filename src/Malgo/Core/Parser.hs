module Malgo.Core.Parser (parse) where

import Data.Char qualified as Char
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Malgo.Core.Syntax hiding (atom, expr, object)
import Malgo.Core.Type
import Malgo.Prelude hiding (space)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | Parse a Core program.
parse :: String -> Text -> Either (ParseErrorBundle Text Void) (Program Text)
parse = Megaparsec.parse do
  space
  program

type Parser = Parsec Void Text

-- | トップレベル宣言
data Def
  = VarDef Text Type (Expr Text)
  | FunDef Text [Text] Type (Expr Text)
  | ExtDef Text Type

-- | Parse a program.
program :: Parser (Program Text)
program = do
  defs <- many $ between (symbol "(") (symbol ")") do
    define <|> extern
  let (topVars, topFuns, extFuns) = foldMap go defs
  pure Program {..}
  where
    define = do
      void $ symbol "define"
      varDef <|> funDef
    varDef = do
      VarDef <$> ident <*> type_ <*> expr
    funDef = do
      f : xs <- between (symbol "(") (symbol ")") (some ident)
      FunDef f xs <$> type_ <*> expr
    extern = do
      void $ symbol "extern"
      ExtDef <$> ident <*> type_
    go (VarDef v t e) = ([(v, t, e)], [], [])
    go (FunDef f xs t e) = ([], [(f, xs, t, e)], [])
    go (ExtDef f t) = ([], [], [(f, t)])

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
      String . convertString <$> (Char.char '"' *> Lexer.charLiteral `manyTill` Char.char '"')
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
              k <- rawIdent
              v <- atom
              pure (k, v)
          )
      pure $ Record $ Map.fromList kvs

-- | Parse an expression.
expr :: Parser (Expr Text)
expr =
  label "expression" do
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
            RawCall <$> rawIdent <*> type_ <*> many atom,
          do
            void $ symbol "cast"
            Cast <$> type_ <*> atom,
          do
            void $ symbol "let"
            Let <$> between (symbol "(") (symbol ")") (many localDef) <*> expr,
          do
            void $ symbol "match"
            Match <$> expr <*> many case_,
          do
            void $ symbol "switch-unboxed"
            SwitchUnboxed
              <$> atom
              <*> some
                ( try $ between (symbol "(") (symbol ")") do
                    (,) <$> (notFollowedBy (symbol "default") >> unboxed) <*> expr
                )
              <*> label
                "default case"
                ( between (symbol "(") (symbol ")") do
                    void $ symbol "default"
                    expr
                ),
          do
            void $ symbol "switch"
            Switch
              <$> atom
              <*> some
                ( try $ between (symbol "(") (symbol ")") do
                    (,) <$> (notFollowedBy (symbol "default") >> tag) <*> expr
                )
              <*> label
                "default case"
                ( between (symbol "(") (symbol ")") do
                    void $ symbol "default"
                    expr
                ),
          do
            void $ try $ symbol "destruct-record"
            DestructRecord
              <$> atom
              <*> between (symbol "(") (symbol ")") (Map.fromList <$> many ((,) <$> rawIdent <*> ident))
              <*> expr,
          do
            void $ symbol "destruct"
            Destruct
              <$> atom
              <*> constructor
              <*> between (symbol "(") (symbol ")") (many ident)
              <*> expr,
          do
            void $ symbol "="
            Assign <$> ident <*> expr <*> expr,
          do
            void $ symbol "ERROR"
            Error <$> type_
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
        OpenRecord (Map.fromList kvs) <$> expr,
      do
        void $ symbol "exact"
        Exact <$> unboxed <*> expr,
      do
        void $ symbol "bind"
        Bind <$> ident <*> type_ <*> expr
    ]

-- | Parse a type.
type_ :: Parser Type
type_ = label "type" do
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
              k <- rawIdent
              v <- type_
              pure (k, v)
            pure $ RecordT $ Map.fromList fs
        ]

-- | Parse a constructor.
constructor :: Parser Con
constructor = between (symbol "(") (symbol ")") do
  tag <- tag
  args <- many type_
  pure $ Con tag args

tag :: Parser Tag
tag = tuple <|> data_
  where
    tuple = void (symbol "Tuple#") >> pure Tuple
    data_ = Data <$> rawIdent

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
symbol :: Text -> Parser ()
symbol = void . Lexer.symbol space

-- | Character that can be used in an identifier.
-- Basically, it is the same as 'Malgo.Parser.identLetter', but we also allow:
-- - '@' for global variables.
-- - '$' for temporary variables.
isIdentStartLetter :: Char -> Bool
isIdentStartLetter =
  \case
    '@' -> True
    '#' -> True
    '$' -> True
    '%' -> True
    _ -> False

isIdentLetter :: Char -> Bool
isIdentLetter c = Char.isAlphaNum c || Set.member c identLetterSet
  where
    identLetterSet :: Set Char
    identLetterSet = Set.fromList "_+-*/\\%=><:;|&!#.@$"

-- | Parse an identifier.
-- In Core, we always know where an identifier appears,
-- so we don't need to check if it is a keyword.
-- (And identifiers that textually look like keywords are allowed.)
ident :: Parser Text
ident = lexeme do
  x <- satisfy isIdentStartLetter
  xs <-
    takeWhile1P Nothing isIdentLetter
      <|> between
        (symbol "[")
        (symbol "]")
        (T.unwords <$> many (lexeme (takeWhile1P Nothing isIdentLetter)))
  pure $ T.cons x $ convertString xs

rawIdent :: Parser Text
rawIdent = lexeme do
  convertString <$> takeWhile1P Nothing isIdentLetter
