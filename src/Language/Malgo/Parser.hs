module Language.Malgo.Parser
  where

import           Language.Malgo.Syntax

import           Data.Functor.Identity (Identity)
import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token     as Tok

lexer = Tok.makeTokenParser $ emptyDef {
    Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.identStart = letter <|> oneOf "!$%&*+-./<=>?@^_~"
  , Tok.identLetter = alphaNum <|> oneOf "!$%&*+-./<=>?@^_~"
  , Tok.reservedOpNames = [":"]
  , Tok.reservedNames = ["#t", "#f"]
  }

integer = Tok.integer lexer
float = Tok.float lexer
parens = Tok.parens lexer
identifier = Tok.identifier lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
brackets = Tok.brackets lexer
lexeme = Tok.lexeme lexer
stringLiteral = Tok.stringLiteral lexer
charLiteral = Tok.charLiteral lexer
symbol = Tok.symbol lexer

parseUntyped = do
  pos <- getPosition
  try (fmap (\name -> XObj (Symbol name) (Just pos) Nothing) identifier)
    <|> try (fmap (\i -> XObj (Int i) (Just pos) (Just IntTy)) integer)
    <|> try (fmap (\f -> XObj (Float f) (Just pos) (Just FloatTy)) float)
    <|> try (reserved "#t" >> return (XObj (Bool True) (Just pos) (Just BoolTy)))
    <|> try (reserved "#f" >> return (XObj (Bool False) (Just pos) (Just BoolTy)))
    <|> try (fmap (\c -> XObj (Char c) (Just pos) (Just CharTy)) charLiteral)
    <|> try (fmap (\s -> XObj (String s) (Just pos) (Just StringTy)) stringLiteral)
    <|> try (char '\'' >> identifier >>= \s -> return (XObj (Tree [XObj (Symbol "quote") Nothing Nothing, XObj (Symbol s) Nothing Nothing]) (Just pos) (Just SymbolTy)))
    <|> fmap (\xs -> XObj (List xs) (Just pos) Nothing) (brackets (many parseExpr))
    <|> fmap (\xs -> XObj (Tree xs) (Just pos) Nothing) (parens (many parseExpr))

parseType =
  try parseAtomTy
    <|> try parseListTy
    <|> parseFuncTy
    <|> parens parseType
  where parseAtomTy = do
          name <- identifier
          case name of
            "Symbol" -> return SymbolTy
            "Int"    -> return IntTy
            "Float"  -> return FloatTy
            "Bool"   -> return BoolTy
            "Char"   -> return CharTy
            "String" -> return StringTy
            a        -> return (AtomTy a)

        parseListTy = do
          symbol "("
          symbol "List"
          ty <- parseType
          symbol ")"
          return (ListTy ty)

        parseFuncTy = do
          symbol "("
          symbol "Func"
          params <- parens (many parseType)
          ret <- parseType
          return (FuncTy params ret)

parseTyped = do
  XObj e pos _ <- parseUntyped
  reservedOp ":"
  ty <- parseType
  return (XObj e pos (Just ty))

parseExpr = try parseTyped <|> parseUntyped

parse = Text.Parsec.parse parseExpr ""
