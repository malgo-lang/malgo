{-# LANGUAGE NoMonomorphismRestriction #-}

module Malgo.NewParser.Pattern
  ( pPat,
    pAtomPat,
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Effectful ((:>))
import Malgo.Features
import Malgo.NewParser.Common
import Malgo.NewParser.Lexer
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec hiding (optional)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

pPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pPat = makeExprParser pAtomPat 
  [ [ Postfix $ manyUnaryOp do
        start <- getSourcePos
        reservedOperator "."
        field <- ident
        end <- getSourcePos
        pure \pat -> ProjectP (Range start end) pat field
    ],
    [ Postfix $ manyUnaryOp do
        start <- getSourcePos
        args <- between (symbol "(") (symbol ")") (sepBy pPat (symbol ","))
        end <- getSourcePos
        pure \pat -> foldl (ApplyP (Range start end)) pat args
    ]
  ]

pAtomPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pAtomPat =
  choice
    [ pThisP,
      pVarP,
      pLiteralP,
      try pBraceTupleP,  -- NEW: Brace tuple patterns {x, y}
      try pRecordP,      -- Records still use braces with = syntax
      pParensP,          -- Parentheses for grouping only
      pListP
    ]

pThisP :: Parser es (Pat (Malgo Parse))
pThisP = do
  start <- getSourcePos
  reservedOperator "#"
  end <- getSourcePos
  pure $ ThisP (Range start end)

pVarP :: Parser es (Pat (Malgo Parse))
pVarP = do
  start <- getSourcePos
  name <- ident
  end <- getSourcePos
  pure $ VarP (Range start end) name

pLiteralP :: Parser es (Pat (Malgo Parse))
pLiteralP = do
  start <- getSourcePos
  boxed <- pBoxed
  sharp <- optional (symbol "#")
  end <- getSourcePos
  case sharp of
    Just _ -> pure $ UnboxedP (Range start end) $ coerce boxed
    Nothing -> pure $ BoxedP (Range start end) boxed

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

-- NEW SYNTAX: Brace tuple patterns {x, y, z}
pBraceTupleP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pBraceTupleP = do
  start <- getSourcePos
  pats <- between (symbol "{") (symbol "}") (sepBy pPat (symbol ","))
  end <- getSourcePos
  case pats of
    [_] -> fail "Single-element tuple patterns are not supported"
    _ -> pure $ TupleP (Range start end) pats

-- Parentheses for grouping patterns only
pParensP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pParensP = between (symbol "(") (symbol ")") pPat

-- Records still use braces but with field = pattern syntax
pRecordP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pRecordP = do
  start <- getSourcePos
  fields <- between (symbol "{") (symbol "}") $ sepEndBy1 pFieldP (symbol ",")
  end <- getSourcePos
  pure $ RecordP (Range start end) fields
  where
    pFieldP = label "record field pattern" do
      field <- ident
      reservedOperator "="
      pattern <- pPat
      pure (field, pattern)

pListP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pListP = do
  start <- getSourcePos
  pats <- between (symbol "[") (symbol "]") (sepBy pPat (symbol ","))
  end <- getSourcePos
  pure $ ListP (Range start end) pats