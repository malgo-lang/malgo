{-# LANGUAGE NoMonomorphismRestriction #-}

module Malgo.Parser.Expression
  ( pExpr,
    pStmts,
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty qualified as NonEmpty
import Effectful ((:>))
import Malgo.Features
import Malgo.Parser.Common
import Malgo.Parser.Lexer
import Malgo.Parser.Pattern (pAtomPat)
import Malgo.Parser.Type (pType)
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

pApply :: (Features :> es) => Parser es (Expr (Malgo Parse))
pApply = do
  cStyleApply <- lift $ hasFeature CStyleApply
  if cStyleApply
    then pCStyleApply
    else
      makeExprParser
        pProject
        [ [ Postfix $ manyUnaryOp do
              start <- getSourcePos
              argument <- pProject
              end <- getSourcePos
              pure \fn -> Apply (Range start end) fn argument
          ]
        ]
  where
    pCStyleApply = do
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
      try pTuple,
      try pRecord,
      pFn,
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

pTuple :: (Features :> es) => Parser es (Expr (Malgo Parse))
pTuple = do
  cStyleApply <- lift $ hasFeature CStyleApply
  if cStyleApply
    then pCStyleTuple
    else normal
  where
    normal = do
      start <- getSourcePos
      exprs <- between (symbol "(") (symbol ")") (sepBy pExpr (symbol ","))
      end <- getSourcePos
      case exprs of
        [expr] ->
          pure $ Seq (Range start end) $ NonEmpty.fromList [NoBind (Range start end) expr]
        _ -> pure $ Tuple (Range start end) exprs
    pCStyleTuple = do
      start <- getSourcePos
      exprs <- between (symbol "{") (symbol "}") (sepBy pExpr (symbol ","))
      end <- getSourcePos
      case exprs of
        [_] -> fail "c-style tuple must have at least two expressions or be empty"
        _ -> pure $ Tuple (Range start end) exprs

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

pFn :: (Features :> es) => Parser es (Expr (Malgo Parse))
pFn = do
  start <- getSourcePos
  clauses <- between (symbol "{") (symbol "}") $ sepEndBy1 pClause (symbol ",")
  end <- getSourcePos
  pure $ Fn (Range start end) $ NonEmpty.fromList clauses

pClause :: (Features :> es) => Parser es (Clause (Malgo Parse))
pClause = do
  start <- getSourcePos
  cStyleApply <- lift $ hasFeature CStyleApply
  patterns <-
    if cStyleApply
      then do
        let withParens = between (symbol "(") (symbol ")") $ sepEndBy pAtomPat (symbol ",")
        let withoutParens = sepEndBy pAtomPat (symbol ",")
        try (withParens <* reservedOperator "->") <|> try (withoutParens <* reservedOperator "->") <|> pure []
      else try (some pAtomPat <* reservedOperator "->") <|> pure []
  body <- pStmts
  end <- getSourcePos
  pure $ Clause (Range start end) patterns body

pStmts :: (Features :> es) => Parser es (Expr (Malgo Parse))
pStmts = do
  start <- getSourcePos
  stmts <- sepEndBy1 pStmt (symbol ";")
  end <- getSourcePos
  pure $ Seq (Range start end) $ NonEmpty.fromList stmts

pStmt :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pStmt = pLet <|> pWith <|> pNoBind

pLet :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pLet = do
  start <- getSourcePos
  reserved "let"
  name <- ident
  reservedOperator "="
  body <- pExpr
  end <- getSourcePos
  pure $ Let (Range start end) name body

pWith :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pWith = do
  start <- getSourcePos
  reserved "with"
  choice
    [ try do
        name <- ident
        reservedOperator "="
        body <- pExpr
        end <- getSourcePos
        pure $ With (Range start end) (Just name) body,
      do
        body <- pExpr
        end <- getSourcePos
        pure $ With (Range start end) Nothing body
    ]

pNoBind :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pNoBind = do
  start <- getSourcePos
  body <- pExpr
  end <- getSourcePos
  pure $ NoBind (Range start end) body

pList :: (Features :> es) => Parser es (Expr (Malgo Parse))
pList = between (symbol "[") (symbol "]") do
  start <- getSourcePos
  elements <- sepEndBy pExpr (symbol ",")
  end <- getSourcePos
  pure $ List (Range start end) elements

pSeq :: (Features :> es) => Parser es (Expr (Malgo Parse))
pSeq = between (symbol "(") (symbol ")") pStmts