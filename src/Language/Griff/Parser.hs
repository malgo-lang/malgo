{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Parser where

import Control.Monad.Combinators.Expr
import qualified Data.Text as T
import Data.Void
import Language.Griff.Extension
import Language.Griff.Syntax
import Language.Malgo.Prelude hiding (many, some)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (($>))

type Parser = Parsec Void Text

-- conbinators

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

identLetter :: Parser Char
identLetter = alphaNumChar <|> oneOf ("_#'" :: String)

opLetter :: Parser Char
opLetter = oneOf ("+-*/%=><:;|&!#" :: String)

pKeyword :: Text -> Parser ()
pKeyword keyword = void $ lexeme (string keyword <* notFollowedBy identLetter)

pOperator :: Text -> Parser ()
pOperator op = void $ lexeme (string op <* notFollowedBy opLetter)

reserved :: Parser ()
reserved = void $ choice $ map (try . pKeyword) ["data", "infixl", "infixr", "infix", "forign", "import"]

reservedOp :: Parser ()
reservedOp = void $ choice $ map (try . pOperator) ["=", "::", "|", "->", ";", ",", "!"]

lowerIdent :: Parser Text
lowerIdent = label "lower identifier" $
  lexeme $ do
    notFollowedBy reserved
    T.pack <$> ((:) <$> (lowerChar <|> char '_') <*> many identLetter)

upperIdent :: Parser Text
upperIdent = label "upper identifier" $
  lexeme $ do
    notFollowedBy reserved
    T.pack <$> ((:) <$> upperChar <*> many identLetter)

operator :: Parser Text
operator = label "operator" $
  lexeme $ do
    notFollowedBy reservedOp
    T.pack <$> some opLetter

-- parser

pUnboxed :: Parser Unboxed
pUnboxed =
  label "unboxed literal" $
    Double <$> try (lexeme $ L.float <* char '#')
      <|> Float <$> try (lexeme $ L.float <* string' "F#")
      <|> Int32 <$> try (lexeme $ L.decimal <* char '#')
      <|> Int64 <$> try (lexeme $ L.decimal <* string' "L#")
      <|> Char <$> lexeme (between (char '\'') (char '\'') L.charLiteral <* char '#')
      <|> String <$> lexeme (char '"' *> manyTill L.charLiteral (char '"') <* char '#')

pVariable :: Parser (Exp (Griff 'Parse))
pVariable =
  label "variable" $
    Var <$> getSourcePos <*> lowerIdent

pConstructor :: Parser (Exp (Griff 'Parse))
pConstructor =
  label "constructor" $
    Con <$> getSourcePos <*> upperIdent

pFun :: Parser (Exp (Griff 'Parse))
pFun =
  label "function literal" $
    between (symbol "{") (symbol "}") $
      Fn <$> getSourcePos
        <*> ( Clause <$> getSourcePos
                <*> (try (some pSinglePat <* pOperator "->") <|> pure [])
                <*> pExpInFn
            )
          `sepBy` pOperator "|"

pSinglePat :: Parser (Pat (Griff 'Parse))
pSinglePat =
  VarP <$> getSourcePos <*> lowerIdent
    <|> ConP <$> getSourcePos <*> upperIdent <*> pure []
    <|> UnboxedP <$> getSourcePos <*> pUnboxed
    <|> between (symbol "(") (symbol ")") pPat

pPat :: Parser (Pat (Griff 'Parse))
pPat =
  label "pattern" $
    try (ConP <$> getSourcePos <*> upperIdent <*> some pSinglePat)
      <|> pSinglePat

pTuple :: Parser (Exp (Griff 'Parse))
pTuple = label "tuple" $
  between (symbol "(") (symbol ")") $
    do
      s <- getSourcePos
      x <- pExp
      pOperator ","
      xs <- pExp `sepBy` pOperator ","
      pure $ Tuple s (x : xs)

pUnit :: Parser (Exp (Griff 'Parse))
pUnit = between (symbol "(") (symbol ")") $ do
  s <- getSourcePos
  pure $ Tuple s []

pSingleExp' :: Parser (Exp (Griff 'Parse))
pSingleExp' =
  Unboxed <$> getSourcePos <*> pUnboxed
    <|> pVariable
    <|> pConstructor
    <|> try pUnit
    <|> try pTuple
    <|> pFun
    <|> between (symbol "(") (symbol ")") pExp

pSingleExp :: Parser (Exp (Griff 'Parse))
pSingleExp =
  try (Force <$> getSourcePos <*> pSingleExp' <* pOperator "!")
    <|> pSingleExp'

pApply :: Parser (Exp (Griff 'Parse))
pApply = do
  s <- getSourcePos
  f <- pSingleExp
  xs <- some pSingleExp
  pure $ foldl (Apply s) f xs

pTerm :: Parser (Exp (Griff 'Parse))
pTerm = try pApply <|> pSingleExp

pOpApp :: Parser (Exp (Griff 'Parse))
pOpApp = makeExprParser pTerm opTable
  where
    opTable =
      [ [ InfixL $ do
            s <- getSourcePos
            op <- operator
            pure $ \l r -> OpApp s op l r
        ]
      ]

pExp :: Parser (Exp (Griff 'Parse))
pExp = pOpApp

pExpInFn :: Parser (Exp (Griff 'Parse))
pExpInFn =
  makeExprParser
    pExp
    [ [ InfixR $ do
          s <- getSourcePos
          pOperator ";"
          pure $ \l r -> Apply s (Fn s [Clause s [VarP s "_"] r]) l
      ]
    ]

pTyVar :: Parser (Type (Griff 'Parse))
pTyVar = label "type variable" $ TyVar <$> getSourcePos <*> lowerIdent

pTyCon :: Parser (Type (Griff 'Parse))
pTyCon =
  label "type constructor" $
    TyCon <$> getSourcePos <*> upperIdent

pTyTuple :: Parser (Type (Griff 'Parse))
pTyTuple = between (symbol "(") (symbol ")") $ do
  s <- getSourcePos
  x <- pType
  pOperator ","
  xs <- pType `sepBy` pOperator ","
  pure $ TyTuple s (x : xs)

pTyUnit :: Parser (Type (Griff 'Parse))
pTyUnit = between (symbol "(") (symbol ")") $ do
  s <- getSourcePos
  pure $ TyTuple s []

pTyLazy :: Parser (Type (Griff 'Parse))
pTyLazy = between (symbol "{") (symbol "}") $
  TyLazy <$> getSourcePos <*> pType

pSingleType :: Parser (Type (Griff 'Parse))
pSingleType = pTyVar <|> pTyCon <|> pTyLazy <|> try pTyUnit <|> try pTyTuple <|> between (symbol "(") (symbol ")") pType

pTyApp :: Parser (Type (Griff 'Parse))
pTyApp = TyApp <$> getSourcePos <*> pSingleType <*> some pSingleType

pTyTerm :: Parser (Type (Griff 'Parse))
pTyTerm = try pTyApp <|> pSingleType

pTyArr :: Parser (Type (Griff 'Parse))
pTyArr = makeExprParser pTyTerm opTable
  where
    opTable =
      [ [ InfixR $ do
            s <- getSourcePos
            void $ pOperator "->"
            pure $ \l r -> TyArr s l r
        ]
      ]

pType :: Parser (Type (Griff 'Parse))
pType = try pTyArr <|> pTyTerm

pScDef :: Parser (Decl (Griff 'Parse))
pScDef =
  label "toplevel function definition" $
    ScDef <$> getSourcePos <*> (lowerIdent <|> between (symbol "(") (symbol ")") operator) <*> many lowerIdent <* pOperator "=" <*> pExp

pScSig :: Parser (Decl (Griff 'Parse))
pScSig =
  label "toplevel function signature" $
    ScSig <$> getSourcePos <*> (lowerIdent <|> between (symbol "(") (symbol ")") operator) <* pOperator "::" <*> pType

pDataDef :: Parser (Decl (Griff 'Parse))
pDataDef = label "toplevel type definition" $ do
  s <- getSourcePos
  void $ pKeyword "data"
  d <- upperIdent
  xs <- many lowerIdent
  void $ pOperator "="
  ts <- pConDef `sepBy` pOperator "|"
  pure $ DataDef s d xs ts
  where
    pConDef = (,) <$> upperIdent <*> many pSingleType

pInfix :: Parser (Decl (Griff 'Parse))
pInfix = label "infix declaration" $ do
  s <- getSourcePos
  a <- try (pKeyword "infixl" $> LeftA) <|> try (pKeyword "infixr" $> RightA) <|> (pKeyword "infix" $> NeutralA)
  i <- lexeme L.decimal
  x <- between (symbol "(") (symbol ")") operator
  pure $ Infix s a i x

pForign :: Parser (Decl (Griff 'Parse))
pForign = label "forign import" $ do
  s <- getSourcePos
  pKeyword "forign"
  pKeyword "import"
  x <- lowerIdent
  pOperator "::"
  Forign s x <$> pType

pDecl :: Parser (Decl (Griff 'Parse))
pDecl = pDataDef <|> pInfix <|> pForign <|> try pScSig <|> pScDef

pTopLevel :: Parser [Decl (Griff 'Parse)]
pTopLevel = pDecl `sepEndBy` pOperator ";" <* eof