{-# LANGUAGE NoMonomorphismRestriction #-}

-- | S-expression for Malgo.
-- The S-expression in this module is a variant of the S-expression in Scheme.
--
-- Some differences are:
-- - There is no character literal.
-- - Identifiers are always enclosed by vertical bars.
module Malgo.SExpr
  ( SExpr (..),
    Node (..),
    parse,
    basicAtom,
    AtomOf,
    ToSExpr (..),
    FromSExpr (..),
  )
where

import Data.Kind (Type)
import Data.Text qualified as T
import Data.Text.ICU.Char
  ( Bool_ (XidContinue, XidStart),
    property,
  )
import Data.Text.ICU.Normalize2 (nfc)
import Malgo.Prelude hiding (sexpr)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data SExpr a = SExpr
  { node :: Node a,
    range :: Range
  }
  deriving stock (Show)

data Node a
  = Atom a
  | Cons (SExpr a) (SExpr a)
  deriving stock (Show)

parse ::
  (Token s ~ Char, IsString (Tokens s), TraversableStream s, Ord e) =>
  Parsec e s a ->
  String ->
  s ->
  Either (ParseErrorBundle s e) (SExpr a)
parse pAtom = Text.Megaparsec.runParser (sexpr pAtom <* eof)

sexpr ::
  (Token s ~ Char, TraversableStream s, IsString (Tokens s), MonadParsec e s m) =>
  m a ->
  m (SExpr a)
sexpr pAtom = do
  start <- getSourcePos
  node <- fmap Atom pAtom <|> cons pAtom <?> "S-expression"
  end <- getSourcePos
  pure SExpr {node, range = Range start end}

cons ::
  (Token s ~ Char, TraversableStream s, IsString (Tokens s), MonadParsec e s m) =>
  m a ->
  m (Node a)
cons pAtom = do
  void $ lexeme $ char '('
  car <- sexpr pAtom
  void $ lexeme $ char '.'
  cdr <- sexpr pAtom
  void $ lexeme $ char ')'
  pure $ Cons car cdr

sc :: (Token s ~ Char, IsString (Tokens s), MonadParsec e s m) => m ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "#|" "|#")

lexeme ::
  (Token s ~ Char, IsString (Tokens s), MonadParsec e s m) =>
  m a ->
  m a
lexeme = L.lexeme sc

data BasicAtom
  = Ident Text
  | Int Integer
  | Real Double
  | String Text
  deriving stock (Show)

-- TODO: specify the lexical syntax of basic atoms
basicAtom :: (Ord e) => ParsecT e Text m BasicAtom
basicAtom =
  asum
    [ ident,
      try $ Real <$> lexeme L.float,
      Int <$> lexeme L.decimal,
      String . convertString <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))
    ]

-- | Parse an identifier.
ident :: (Token s ~ Char, MonadParsec e s f) => f BasicAtom
ident =
  Ident <$> xid
  where
    xid = do
      c <- satisfy (property XidStart) <|> char '_'
      cs <- many (satisfy (property XidContinue))
      pure $ nfc $ T.pack (c : cs)

type family AtomOf a :: Type

class ToSExpr a where
  toSExpr :: a -> SExpr (AtomOf a)

class FromSExpr a where
  fromSExpr :: SExpr (AtomOf a) -> Maybe a
