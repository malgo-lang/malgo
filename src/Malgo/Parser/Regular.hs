module Malgo.Parser.Regular (parseRegular) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.Lazy qualified as TL
import Effectful (Eff, IOE, type (:>))
import Malgo.Features (Features)
import Malgo.Module (ModuleName (..), Workspace, parseArtifactPath, pwdPath)
import Malgo.Parser.Core
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec hiding (optional, parse)

-- | parseRegular parses traditional Malgo syntax without C-style features
parseRegular :: (IOE :> es, Workspace :> es, Features :> es) => FilePath -> TL.Text -> Eff es (Either (ParseErrorBundle TL.Text Void) (Module (Malgo Parse)))
parseRegular srcPath text = do
  runParserT (pModule <* eof) srcPath text

-- | pModule parses a complete module using regular syntax
pModule :: (IOE :> es, Workspace :> es, Features :> es) => Parser es (Module (Malgo Parse))
pModule = do
  space -- consume leading whitespace and comments
  sourcePath <- (.sourceName) <$> getSourcePos
  pwd <- lift pwdPath
  sourcePath <- lift $ parseArtifactPath pwd sourcePath
  decls <- many pDecl
  pure
    Module
      { moduleName = Artifact sourcePath,
        moduleDefinition = ParsedDefinitions decls
      }

-- | pDecl parses declarations with regular expression syntax
pDecl :: (IOE :> es, Workspace :> es, Features :> es) => Parser es (Decl (Malgo Parse))
pDecl = do
  _ <- many skipPragma
  choice
    [ pDataDef,
      pTypeSynonym,
      pInfix,
      pForeign,
      pImport,
      try pScSig, -- must be before pScDef
      pScDef
    ]

-- | pDataDef parses a data definition.
--
-- > dataDef = "data" ident ident* "=" constructor ("|" constructor)* ;
-- > constructor = ident atomType* ;
pDataDef :: Parser es (Decl (Malgo Parse))
pDataDef = captureRange do
  reserved "data"
  name <- ident
  parameters <- many do
    captureRange do
      parameter <- ident
      pure (,parameter)
  reservedOperator "="
  constructors <- sepBy1 pConstructor (reservedOperator "|")
  pure $ \range -> DataDef range name parameters constructors
  where
    pConstructor = captureRange do
      name <- ident
      parameters <- many pAtomType
      pure (,name,parameters)

-- | pTypeSynonym parses a type synonym.
--
-- > typeSynonym = "type" ident ident* "=" type ;
pTypeSynonym :: Parser es (Decl (Malgo Parse))
pTypeSynonym = captureRange do
  reserved "type"
  name <- ident
  parameters <- many ident
  reservedOperator "="
  ty <- pType
  pure $ \range -> TypeSynonym range name parameters ty

-- | pScDef parses value definitions with regular expression syntax
pScDef :: (Features :> es) => Parser es (Decl (Malgo Parse))
pScDef = captureRange do
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator "="
  body <- pExpr
  pure $ \range -> ScDef range name body

-- | pExpr parses expressions with regular syntax (no C-style)
pExpr :: (Features :> es) => Parser es (Expr (Malgo Parse))
pExpr = do
  expr <- pOpApp
  -- try parse a type annotation
  try (pAnn expr) <|> pure expr
  where
    pAnn expr = captureRange do
      reservedOperator ":"
      ty <- pType
      pure $ \range -> Ann range expr ty

-- | pOpApp parses operator application with regular syntax
pOpApp :: (Features :> es) => Parser es (Expr (Malgo Parse))
pOpApp = makeExprParser pApply table
  where
    table =
      [ [ InfixL do
            captureRange do
              op <- operator
              pure $ \range -> OpApp range op
        ]
      ]

-- | pApply parses traditional space-separated function application
pApply :: (Features :> es) => Parser es (Expr (Malgo Parse))
pApply =
  makeExprParser
    pProject
    [ [ Postfix $ manyUnaryOp do
          captureRange do
            argument <- pProject
            pure $ \range fn -> Apply range fn argument
      ]
    ]

-- | pProject parses field projection
pProject :: (Features :> es) => Parser es (Expr (Malgo Parse))
pProject =
  makeExprParser
    pAtom
    [ [ Postfix $ manyUnaryOp do
          captureRange do
            reservedOperator "."
            field <- ident
            pure $ \range record -> Project range record field
      ]
    ]

-- | pAtom parses atoms without C-style syntax
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

-- | pTuple parses traditional tuple syntax with parentheses
-- > tuple = "(" expr ("," expr)* ")" | "(" ")"
pTuple :: (Features :> es) => Parser es (Expr (Malgo Parse))
pTuple = captureRange do
  exprs <- between (symbol "(") (symbol ")") (sepBy pExpr (symbol ","))
  case exprs of
    [expr] -> pure $ \range -> Parens range expr
    _ -> pure $ \range -> Tuple range exprs

-- | pRecord parses record syntax (traditional)
pRecord :: (Features :> es) => Parser es (Expr (Malgo Parse))
pRecord = captureRange do
  fields <- between (symbol "{") (symbol "}") $ sepEndBy1 pField (symbol ",")
  pure $ \range -> Record range fields
  where
    pField = label "record field" do
      field <- ident
      reservedOperator "="
      value <- pExpr
      pure (field, value)

-- | pFn parses function syntax without C-style clauses
pFn :: (Features :> es) => Parser es (Expr (Malgo Parse))
pFn = captureRange do
  clauses <- between (symbol "{") (symbol "}") $ sepEndBy1 pClause (symbol ",")
  pure $ \range -> Fn range $ NonEmpty.fromList clauses

-- | pClause parses traditional clauses without parentheses
-- > clause = pattern+ "->" stmts
pClause :: (Features :> es) => Parser es (Clause (Malgo Parse))
pClause = captureRange do
  patterns <- try (some pAtomPat <* reservedOperator "->") <|> pure []
  body <- pStmts
  pure $ \range -> Clause range patterns body

-- | pAtomPat parses atomic patterns
pAtomPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pAtomPat =
  choice
    [ pVarP,
      pLiteralP,
      try pTupleP,
      pRecordP,
      pListP
    ]

-- | pTupleP parses traditional tuple patterns with parentheses
pTupleP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pTupleP = captureRange do
  patterns <- between (symbol "(") (symbol ")") (sepBy pPat (symbol ","))
  case patterns of
    [pattern] -> pure $ const pattern -- Just return the single pattern, no ParenP wrapper
    _ -> pure $ \range -> TupleP range patterns

-- | pPat parses patterns
pPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pPat = try pConP <|> pAtomPat

-- | pConP parses constructor patterns
pConP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pConP = captureRange do
  constructor <- ident
  patterns <- some pAtomPat
  pure $ \range -> ConP range constructor patterns

-- | pStmts parses statements using regular syntax
pStmts :: (Features :> es) => Parser es (Expr (Malgo Parse))
pStmts = captureRange do
  stmts <- sepEndBy1 pStmt (symbol ";")
  pure $ \range -> Seq range $ NonEmpty.fromList stmts

-- | pStmt parses statements using regular syntax
pStmt :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pStmt = pLet <|> pWith <|> pNoBind

-- | pLet parses let statements using regular syntax
pLet :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pLet = captureRange do
  reserved "let"
  name <- ident
  reservedOperator "="
  body <- pExpr
  pure $ \range -> Let range name body

-- | pWith parses with statements using regular syntax
pWith :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pWith = captureRange do
  reserved "with"
  choice
    [ try do
        name <- ident
        reservedOperator "="
        body <- pExpr
        pure $ \range -> With range (Just name) body,
      do
        body <- pExpr
        pure $ \range -> With range Nothing body
    ]

-- | pNoBind parses no-bind statements using regular syntax
pNoBind :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pNoBind = captureRange do
  body <- pExpr
  pure $ \range -> NoBind range body

-- | pSeq parses parenthesized sequences using regular syntax
pSeq :: (Features :> es) => Parser es (Expr (Malgo Parse))
pSeq = between (symbol "(") (symbol ")") pStmts

-- | pList parses list literals using regular syntax
pList :: (Features :> es) => Parser es (Expr (Malgo Parse))
pList = between (symbol "[") (symbol "]") do
  captureRange do
    elements <- sepEndBy pExpr (symbol ",")
    pure $ \range -> List range elements

-- | pRecordP parses record patterns using regular syntax
pRecordP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pRecordP = captureRange do
  fields <- between (symbol "{") (symbol "}") $ sepEndBy1 pField (symbol ",")
  pure $ \range -> RecordP range fields
  where
    pField = do
      field <- ident
      reservedOperator "="
      value <- pPat
      pure (field, value)

-- | pListP parses list patterns using regular syntax
pListP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pListP = captureRange do
  pats <- between (symbol "[") (symbol "]") $ sepEndBy pPat (symbol ",")
  pure $ \range -> ListP range pats
