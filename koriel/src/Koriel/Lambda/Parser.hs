{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Koriel.Lambda.Parser where

import qualified Data.Text.IO as T
import Data.Void
import Koriel.Lambda.Syntax
import Koriel.Prelude hiding (many, some)
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

parseKorielFromFile :: FilePath -> IO [Stmt (Koriel 'Raw)]
parseKorielFromFile fileName = do
  srcText <- T.readFile fileName
  pure $ parseKoriel fileName srcText

parseKoriel :: FilePath -> Text -> [Stmt (Koriel 'Raw)]
parseKoriel fileName srcText = case parse (many pStmt <* eof :: Parsec Void Text [Stmt (Koriel 'Raw)]) fileName srcText of
  Right ast -> ast
  Left err -> error $ errorBundlePretty err

-- Stmt
pStmt :: (MonadParsec e s f, IsString (Tokens s), Token s ~ Char) => f (Stmt (Koriel 'Raw))
pStmt = try pDefVar <|> pDefType

pDefVar :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Stmt (Koriel 'Raw))
pDefVar = parens do
  _ <- symbol "defvar"
  DefVar () <$> pId <*> pExp

pDefType :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Stmt (Koriel 'Raw))
pDefType = parens do
  _ <- symbol "deftype"
  DefType () <$> pId <*> pType

-- Exp
pExp :: (MonadParsec e s f, Token s ~ Char, IsString (Tokens s)) => f (Exp (Koriel 'Raw))
pExp =
  try pAtom
    <|> try pVar
    <|> try pLam
    <|> try pApp
    <|> try pTLam
    <|> try pTApp
    <|> try pTag
    <|> try pCase
    <|> try pRecord
    <|> pProj

pAtom :: (MonadParsec e s f, Token s ~ Char, IsString (Tokens s)) => f (Exp (Koriel 'Raw))
pAtom =
  Atom () <$> asum [try int32, try int64, try float, try double, try char, string]
  where
    int32 = parens do
      _ <- symbol "int32"
      Int32 <$> L.signed sc (lexeme L.decimal)
    int64 = parens do
      _ <- symbol "int64"
      Int64 <$> L.signed sc (lexeme L.decimal)
    float = parens do
      _ <- symbol "float"
      Float <$> L.signed sc (lexeme L.float)
    double = parens do
      _ <- symbol "double"
      Double <$> L.signed sc (lexeme L.float)
    char = parens do
      _ <- symbol "char"
      Char <$> between (symbol "'") (symbol "'") L.charLiteral
    string = parens do
      _ <- symbol "string"
      String <$> stringLiteral

stringLiteral :: (MonadParsec e s m, Token s ~ Char) => m [Char]
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

pVar :: (MonadParsec e s f, Token s ~ Char, IsString (Tokens s)) => f (Exp (Koriel 'Raw))
pVar = Var () <$> pId

pLam :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Exp (Koriel 'Raw))
pLam = parens do
  _ <- symbol "lam"
  x <- pId
  t <- pType
  Lam () x t <$> pExp

pApp :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Exp (Koriel 'Raw))
pApp = parens $ App () <$> pExp <*> pExp

pTLam :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Exp (Koriel 'Raw))
pTLam = parens do
  _ <- symbol "tlam"
  x <- pId
  k <- pKind
  TLam () x k <$> pExp

pTApp :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Exp (Koriel 'Raw))
pTApp = brackets $ TApp () <$> pExp <*> pType

pTag :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Exp (Koriel 'Raw))
pTag = between (symbol "<") (symbol ">") $ Tag () <$> pId <*> pExp <*> pType

pCase :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Exp (Koriel 'Raw))
pCase = parens do
  _ <- symbol "case"
  Case () <$> pExp <*> parens (some (parens $ (,,) <$> pId <*> pId <*> pExp))

pRecord :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Exp (Koriel 'Raw))
pRecord =
  braces $ Record () <$> many (parens $ (,) <$> pId <*> pExp)

pProj :: (MonadParsec e s f, IsString (Tokens s), Token s ~ Char) => f (Exp (Koriel 'Raw))
pProj = parens do
  _ <- symbol "proj"
  Proj () <$> pExp <*> pId

-- Type

pType :: (MonadParsec e s f, Token s ~ Char, IsString (Tokens s)) => f (Type (Koriel 'Raw))
pType =
  try (TyAtom () <$> pTyAtom)
    <|> try pTyVar
    <|> try pTyArr
    <|> try pTyAll
    <|> try pTyAbs
    <|> try pTyRecord
    <|> pTyVariant

pTyAtom :: (MonadParsec e s f, IsString (Tokens s), Token s ~ Char) => f (TyAtom x)
pTyAtom =
  try (symbol "Int32" >> pure TyInt32)
    <|> try (symbol "Int64" >> pure TyInt64)
    <|> try (symbol "Float" >> pure TyFloat)
    <|> try (symbol "Double" >> pure TyDouble)
    <|> try (symbol "Char" >> pure TyChar)
    <|> (symbol "String" >> pure TyString)

pTyVar :: (MonadParsec e s f, Token s ~ Char, IsString (Tokens s)) => f (Type (Koriel 'Raw))
pTyVar = TyVar () <$> pId

pTyArr :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Type (Koriel 'Raw))
pTyArr = parens do
  _ <- symbol "->"
  TyArr () <$> pType <*> pType

pTyAll :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Type (Koriel 'Raw))
pTyAll = parens do
  _ <- symbol "forall"
  TyAll () <$> pId <*> pKind <*> pType

pTyAbs :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Type (Koriel 'Raw))
pTyAbs = parens do
  _ <- symbol "lam"
  TyAbs () <$> pId <*> pKind <*> pType

pTyApp :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Type (Koriel 'Raw))
pTyApp = parens do
  TyApp () <$> pType <*> pType

pTyRecord :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Type (Koriel 'Raw))
pTyRecord = braces $ TyRecord () <$> many (parens $ (,) <$> pId <*> pType)

pTyVariant :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Type (Koriel 'Raw))
pTyVariant = between (symbol "<") (symbol ">") $ TyVariant () <$> many (parens $ (,) <$> pId <*> pType)

-- Kind

pKind :: (MonadParsec e s f, IsString (Tokens s), Token s ~ Char) => f (Kind (Koriel 'Raw))
pKind =
  try (symbol "Box" >> pure (KStar () KBox))
    <|> try (symbol "Int32" >> pure (KStar () KInt32))
    <|> try (symbol "Int64" >> pure (KStar () KInt64))
    <|> try (symbol "Float" >> pure (KStar () KFloat))
    <|> try (symbol "Double" >> pure (KStar () KDouble))
    <|> try (symbol "Char" >> pure (KStar () KChar))
    <|> try (symbol "String" >> pure (KStar () KString))
    <|> parens (symbol "->" >> KArr () <$> pKind <*> pKind)

-- Id
pId :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m [Char]
pId = lexeme (char '`' >> someTill L.charLiteral (char '`'))

-- Combinators
parens :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m a -> m a
parens = between (symbol "(") (symbol ")")

brackets :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m a -> m a
brackets = between (symbol "[") (symbol "]")

braces :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m a -> m a
braces = between (symbol "{") (symbol "}")

sc :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m a -> m a
lexeme = L.lexeme sc

symbol :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => Tokens s -> m (Tokens s)
symbol = L.symbol sc
