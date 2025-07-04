{-# LANGUAGE NoMonomorphismRestriction #-}

module Malgo.Parser.Pattern
  ( pPat,
    pAtomPat,
  )
where

import Control.Monad.Trans (lift)
import Effectful ((:>))
import Malgo.Features
import Malgo.Parser.Common
import Malgo.Parser.Lexer
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec hiding (optional)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

pPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pPat = try pConP <|> pAtomPat

pConP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pConP = do
  start <- getSourcePos
  name <- ident
  pats <- some pAtomPat
  end <- getSourcePos
  pure $ ConP (Range start end) name pats

pAtomPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pAtomPat =
  choice
    [ pVarP,
      pLiteralP,
      try pTupleP,
      pRecordP,
      pListP
    ]

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

pTupleP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pTupleP = do
  cStyleApply <- lift $ hasFeature CStyleApply
  if cStyleApply
    then pCStyleTupleP
    else normal
  where
    normal = do
      start <- getSourcePos
      pats <- between (symbol "(") (symbol ")") (sepBy pPat (symbol ","))
      end <- getSourcePos
      case pats of
        [pat] -> pure pat
        _ -> pure $ TupleP (Range start end) pats
    pCStyleTupleP = do
      start <- getSourcePos
      pats <- between (symbol "{") (symbol "}") (sepBy pPat (symbol ","))
      end <- getSourcePos
      case pats of
        [_] -> fail "c-style tuple must have at least two patterns or be empty"
        _ -> pure $ TupleP (Range start end) pats

pRecordP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pRecordP = do
  start <- getSourcePos
  fields <- between (symbol "{") (symbol "}") $ sepEndBy1 pField (symbol ",")
  end <- getSourcePos
  pure $ RecordP (Range start end) fields
  where
    pField = do
      field <- ident
      reservedOperator "="
      value <- pPat
      pure (field, value)

pListP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pListP = do
  start <- getSourcePos
  pats <- between (symbol "[") (symbol "]") $ sepEndBy pPat (symbol ",")
  end <- getSourcePos
  pure $ ListP (Range start end) pats