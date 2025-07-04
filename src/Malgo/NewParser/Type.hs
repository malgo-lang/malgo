{-# LANGUAGE NoMonomorphismRestriction #-}

module Malgo.NewParser.Type
  ( pType,
    pAtomType,
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

pType :: (Features :> es) => Parser es (Type (Malgo Parse))
pType = makeExprParser pApplyType
  [ [ InfixR do
        start <- getSourcePos
        reservedOperator "->"
        end <- getSourcePos
        pure $ TyArr (Range start end)
    ]
  ]

-- NEW SYNTAX: Type applications List(a) or Map(a, b)
pApplyType :: (Features :> es) => Parser es (Type (Malgo Parse))
pApplyType = makeExprParser pAtomType
  [ [ Postfix $ manyUnaryOp do
        start <- getSourcePos
        args <- between (symbol "(") (symbol ")") (sepBy pType (symbol ","))
        end <- getSourcePos
        pure \ty -> TyApp (Range start end) ty args
    ]
  ]

pAtomType :: (Features :> es) => Parser es (Type (Malgo Parse))
pAtomType =
  choice
    [ pTyVar,
      try pBraceTupleType,  -- NEW: Brace tuple types {Int, String}
      try pRecordType,      -- Records still use braces with : syntax
      pParensType,          -- Parentheses for grouping only
      pBlockType
    ]

pTyVar :: Parser es (Type (Malgo Parse))
pTyVar = do
  start <- getSourcePos
  name <- ident
  end <- getSourcePos
  pure $ TyVar (Range start end) name

-- NEW SYNTAX: Brace tuple types {a, b, c}
pBraceTupleType :: (Features :> es) => Parser es (Type (Malgo Parse))
pBraceTupleType = do
  start <- getSourcePos
  types <- between (symbol "{") (symbol "}") (sepBy pType (symbol ","))
  end <- getSourcePos
  case types of
    [_] -> fail "Single-element tuple types are not supported"
    _ -> pure $ TyTuple (Range start end) types

-- Parentheses for grouping types only
pParensType :: (Features :> es) => Parser es (Type (Malgo Parse))
pParensType = between (symbol "(") (symbol ")") pType

-- Records still use braces but with field : type syntax
pRecordType :: (Features :> es) => Parser es (Type (Malgo Parse))
pRecordType = do
  start <- getSourcePos
  fields <- between (symbol "{") (symbol "}") $ sepEndBy1 pFieldType (symbol ",")
  end <- getSourcePos
  pure $ TyRecord (Range start end) fields
  where
    pFieldType = label "record field type" do
      field <- ident
      reservedOperator ":"
      ty <- pType
      pure (field, ty)

pBlockType :: (Features :> es) => Parser es (Type (Malgo Parse))
pBlockType = do
  start <- getSourcePos
  reserved "type"
  ty <- pType
  end <- getSourcePos
  pure $ TyBlock (Range start end) ty