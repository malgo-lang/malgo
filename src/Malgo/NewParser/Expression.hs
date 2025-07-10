{-# LANGUAGE NoMonomorphismRestriction #-}

module Malgo.NewParser.Expression
  ( pExpr,
    pStmts,
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List.NonEmpty qualified as NonEmpty
import Effectful ((:>))
import Malgo.Features
import Malgo.NewParser.Common
import Malgo.NewParser.Lexer
import Malgo.NewParser.Pattern (pAtomPat)
import Malgo.NewParser.Type (pType)
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec hiding (optional)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

pExpr :: (Features :> es) => Parser es (Expr (Malgo Parse))
pExpr = do
  start <- getSourcePos
  expr <- pOpApp
  try (pAnn start expr) <|> pure expr
  where
    pAnn start expr = do
      reservedOperator ":"
      ty <- pType
      end <- getSourcePos
      pure $ Ann (Range start end) expr ty

pOpApp :: (Features :> es) => Parser es (Expr (Malgo Parse))
pOpApp = makeExprParser pApply table
  where
    table =
      [ [ InfixL do
            start <- getSourcePos
            op <- operator
            end <- getSourcePos
            pure $ OpApp (Range start end) op
        ]
      ]

-- NEW SYNTAX: C-style function application f(x, y, z)
pApply :: (Features :> es) => Parser es (Expr (Malgo Parse))
pApply =
  makeExprParser
    pProject
    [ [ Postfix $ manyUnaryOp do
          start <- getSourcePos
          args <- between (symbol "(") (symbol ")") (sepBy pExpr (symbol ","))
          end <- getSourcePos
          pure \fn -> foldl (Apply (Range start end)) fn args
      ]
    ]

pProject :: (Features :> es) => Parser es (Expr (Malgo Parse))
pProject =
  makeExprParser
    pAtom
    [ [ Postfix $ manyUnaryOp do
          start <- getSourcePos
          reservedOperator "."
          field <- ident
          end <- getSourcePos
          pure \record -> Project (Range start end) record field
      ]
    ]

pAtom :: (Features :> es) => Parser es (Expr (Malgo Parse))
pAtom =
  choice
    [ pLiteral,
      pVariable,
      try pBraceTuple, -- NEW: Brace expressions - functions or tuples
      try pRecord, -- Records still use braces but with = syntax
      pParens, -- Parentheses for grouping only
      pList,
      pSeq
    ]

pLiteral :: Parser es (Expr (Malgo Parse))
pLiteral = do
  start <- getSourcePos
  boxed <- pBoxed
  sharp <- optional (symbol "#")
  end <- getSourcePos
  case sharp of
    Just _ -> pure $ Unboxed (Range start end) $ coerce boxed
    Nothing -> pure $ Boxed (Range start end) boxed

pBoxed :: Parser es (Literal Boxed)
pBoxed = choice [try pReal, pInt, pChar, pString]

pReal :: Parser es (Literal Boxed)
pReal = lexeme do
  f <- L.float
  tail <- optional (char 'f' <|> char 'F')
  case tail of
    Just _ -> pure $ Float (realToFrac f)
    Nothing -> pure $ Double f

pInt :: Parser es (Literal Boxed)
pInt = lexeme do
  i <- L.decimal
  tail <- optional (char 'l' <|> char 'L')
  case tail of
    Just _ -> pure $ Int64 i
    Nothing -> pure $ Int32 (fromIntegral i)

pChar :: Parser es (Literal Boxed)
pChar = lexeme do
  void $ char '\''
  c <- L.charLiteral
  void $ char '\''
  pure $ Char c

pString :: Parser es (Literal Boxed)
pString = String <$> pStringLiteral

pVariable :: Parser es (Expr (Malgo Parse))
pVariable = do
  start <- getSourcePos
  name <- ident
  end <- getSourcePos
  pure $ Var (Range start end) name

-- NEW SYNTAX: Brace expressions - function blocks or tuples
pBraceTuple :: (Features :> es) => Parser es (Expr (Malgo Parse))
pBraceTuple = do
  start <- getSourcePos
  content <-
    between (symbol "{") (symbol "}")
      $ try pFunctionClauses
      <|> pExpressionSequence
  end <- getSourcePos
  case content of
    Left clauses -> pure $ Fn (Range start end) (NonEmpty.fromList clauses)
    Right exprs -> case exprs of
      [] -> pure $ Fn (Range start end) (NonEmpty.fromList [Clause (Range start end) [] (Seq (Range start end) (NonEmpty.fromList [NoBind (Range start end) (Tuple (Range start end) [])]))])
      [expr] -> pure $ Fn (Range start end) (NonEmpty.fromList [Clause (Range start end) [] (Seq (Range start end) (NonEmpty.fromList [NoBind (Range start end) expr]))])
      _ -> pure $ Tuple (Range start end) exprs
  where
    -- Try to parse function clauses - must have at least one arrow
    pFunctionClauses = do
      clauses <- sepEndBy1 pClause (symbol ",")
      pure $ Left clauses

    pClause = do
      start <- getSourcePos
      patterns <-
        try (between (symbol "(") (symbol ")") $ sepEndBy pAtomPat (symbol ","))
          <|> try (sepEndBy pAtomPat (symbol ","))
          <|> pure []
      reservedOperator "->"
      bodyExpr <- pStmtsAsExpr
      end <- getSourcePos
      pure $ Clause (Range start end) patterns bodyExpr

    -- Parse as expression sequence (could be statements separated by ; or expressions separated by ,)
    pExpressionSequence = do
      exprStart <- getSourcePos
      stmts <- pStmts
      exprEnd <- getSourcePos
      pure $ Right [Seq (Range exprStart exprEnd) stmts]

-- Parentheses for grouping expressions only
pParens :: (Features :> es) => Parser es (Expr (Malgo Parse))
pParens = do
  start <- getSourcePos
  expr <- between (symbol "(") (symbol ")") pExpr
  end <- getSourcePos
  pure $ Seq (Range start end) $ NonEmpty.fromList [NoBind (Range start end) expr]

-- Records still use braces but with field = value syntax
pRecord :: (Features :> es) => Parser es (Expr (Malgo Parse))
pRecord = do
  start <- getSourcePos
  fields <- between (symbol "{") (symbol "}") $ sepEndBy1 pField (symbol ",")
  end <- getSourcePos
  pure $ Record (Range start end) fields
  where
    pField = label "record field" do
      field <- ident
      reservedOperator "="
      value <- pExpr
      pure (field, value)

pList :: (Features :> es) => Parser es (Expr (Malgo Parse))
pList = do
  start <- getSourcePos
  items <- between (symbol "[") (symbol "]") (sepBy pExpr (symbol ","))
  end <- getSourcePos
  pure $ List (Range start end) items

pSeq :: (Features :> es) => Parser es (Expr (Malgo Parse))
pSeq = do
  start <- getSourcePos
  stmts <- between (symbol "(") (symbol ")") pStmts
  end <- getSourcePos
  pure $ Seq (Range start end) stmts

pStmts :: (Features :> es) => Parser es (NonEmpty (Stmt (Malgo Parse)))
pStmts = NonEmpty.fromList <$> sepEndBy1 pStmt (symbol ";")

-- Helper function to parse statements and return them as a Seq expression
pStmtsAsExpr :: (Features :> es) => Parser es (Expr (Malgo Parse))
pStmtsAsExpr = do
  start <- getSourcePos
  stmts <- pStmts
  end <- getSourcePos
  pure $ Seq (Range start end) stmts

pStmt :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pStmt = choice [try pLet, try pWith, pNoBind]

pLet :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pLet = do
  start <- getSourcePos
  reserved "let"
  name <- ident
  reservedOperator "="
  value <- pExpr
  end <- getSourcePos
  pure $ Let (Range start end) name value

pWith :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pWith = do
  start <- getSourcePos
  reserved "with"
  choice
    [ do
        name <- ident
        reservedOperator "="
        value <- pExpr
        end <- getSourcePos
        pure $ With (Range start end) (Just name) value,
      do
        value <- pExpr
        end <- getSourcePos
        pure $ With (Range start end) Nothing value
    ]

pNoBind :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pNoBind = do
  start <- getSourcePos
  expr <- pExpr
  end <- getSourcePos
  pure $ NoBind (Range start end) expr