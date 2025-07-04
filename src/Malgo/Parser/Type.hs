module Malgo.Parser.Type
  ( pType,
    pAtomType,
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Malgo.Parser.Common
import Malgo.Parser.Lexer
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec

pType :: Parser es (Type (Malgo Parse))
pType = makeExprParser pTyApp table
  where
    table =
      [ [ InfixR do
            start <- getSourcePos
            reservedOperator "->"
            TyArr . Range start <$> getSourcePos
        ]
      ]

pTyApp :: Parser es (Type (Malgo Parse))
pTyApp = do
  start <- getSourcePos
  ty <- pAtomType
  tys <- many pAtomType
  end <- getSourcePos
  case tys of
    [] -> pure ty
    _ -> pure $ TyApp (Range start end) ty tys

pAtomType :: Parser es (Type (Malgo Parse))
pAtomType = choice [pTyVar, pTyTuple, try pTyRecord, pTyBlock]

pTyVar :: Parser es (Type (Malgo Parse))
pTyVar = do
  start <- getSourcePos
  name <- ident
  end <- getSourcePos
  pure $ TyVar (Range start end) name

pTyTuple :: Parser es (Type (Malgo Parse))
pTyTuple = do
  start <- getSourcePos
  tys <- between (symbol "(") (symbol ")") (sepBy pType (symbol ","))
  end <- getSourcePos
  case tys of
    [ty] -> pure ty
    _ -> pure $ TyTuple (Range start end) tys

pTyRecord :: Parser es (Type (Malgo Parse))
pTyRecord = do
  start <- getSourcePos
  fields <- between (symbol "{") (symbol "}") $ sepEndBy1 pField (symbol ",")
  end <- getSourcePos
  pure $ TyRecord (Range start end) fields
  where
    pField = do
      field <- ident
      reservedOperator ":"
      value <- pType
      pure (field, value)

pTyBlock :: Parser es (Type (Malgo Parse))
pTyBlock = do
  start <- getSourcePos
  ty <- between (symbol "{") (symbol "}") pType
  end <- getSourcePos
  pure $ TyBlock (Range start end) ty