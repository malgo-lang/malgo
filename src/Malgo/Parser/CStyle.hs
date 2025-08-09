module Malgo.Parser.CStyle (parseCStyle) where

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

-- | parseCStyle parses C-style apply syntax with parentheses and braces
parseCStyle :: (IOE :> es, Workspace :> es, Features :> es) => FilePath -> TL.Text -> Eff es (Either (ParseErrorBundle TL.Text Void) (Module (Malgo Parse)))
parseCStyle srcPath text = do
  runParserT (pModule <* eof) srcPath text

-- | pModule parses a complete module using C-style syntax
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

-- | pDecl parses declarations with C-style expression syntax
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

-- | pScDef parses value definitions with C-style expression syntax
pScDef :: (Features :> es) => Parser es (Decl (Malgo Parse))
pScDef = captureRange do
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator "="
  body <- pExpr
  pure $ \range -> ScDef range name body

-- | pCStyleDataDef parses C-style data definitions with parenthesized parameters
-- > cStyleDataDef = "data" ident "(" (ident ("," ident)*)? ")" "=" constructor ("|" constructor)* ;
-- > constructor = ident "(" (atomType ("," atomType)*)? ")" ;
pDataDef :: Parser es (Decl (Malgo Parse))
pDataDef = captureRange do
  reserved "data"
  name <- ident
  parameters <- pParameterList
  reservedOperator "="
  constructors <- sepBy1 pCStyleConstructor (reservedOperator "|")
  pure $ \range -> DataDef range name parameters constructors
  where
    pCStyleConstructor = captureRange do
      name <- ident
      parameters <- pConstructorParams
      pure (,name,parameters)

-- | pCStyleTypeSynonym parses C-style type synonyms with parenthesized parameters
-- > cStyleTypeSynonym = "type" ident "(" (ident ("," ident)*)? ")" "=" type ;
pTypeSynonym :: Parser es (Decl (Malgo Parse))
pTypeSynonym = captureRange do
  reserved "type"
  name <- ident
  parameters <- map snd <$> pParameterList
  reservedOperator "="
  ty <- pType
  pure $ \range -> TypeSynonym range name parameters ty

-- | pParameterList parses comma-separated parameters in parentheses
pParameterList :: Parser es [(Range, Text)]
pParameterList = between (symbol "(") (symbol ")") (sepBy pParameter (symbol ","))
  where
    pParameter = captureRange do
      param <- ident
      pure (,param)

-- | pConstructorParams parses comma-separated types in parentheses
pConstructorParams :: Parser es [Type (Malgo Parse)]
pConstructorParams = between (symbol "(") (symbol ")") (sepBy pType (symbol ","))

-- | pExpr parses expressions with C-style syntax
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

-- | pOpApp parses operator application with C-style syntax
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

-- | pApply parses C-style function application with parentheses
pApply :: (Features :> es) => Parser es (Expr (Malgo Parse))
pApply =
  makeExprParser
    pProject
    [ [ Postfix $ manyUnaryOp do
          captureRange do
            args <- between (symbol "(") (symbol ")") (sepBy pExpr (symbol ","))
            pure $ \range fn -> foldl (Apply range) fn args
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

-- | pAtom parses atoms with C-style syntax
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

-- | pTuple parses C-style tuple syntax with braces
-- > tuple = "{" expr ("," expr)+ "}" | "{" "}"
pTuple :: (Features :> es) => Parser es (Expr (Malgo Parse))
pTuple = captureRange do
  exprs <- between (symbol "{") (symbol "}") (sepBy pExpr (symbol ","))
  case exprs of
    [_] -> fail "c-style tuple must have at least two expressions or be empty"
    _ -> pure $ \range -> Tuple range exprs

-- | pRecord parses record syntax (separate from tuples in C-style)
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

-- | pFn parses function syntax with C-style clauses
pFn :: (Features :> es) => Parser es (Expr (Malgo Parse))
pFn = captureRange do
  clauses <- between (symbol "{") (symbol "}") $ sepEndBy1 pClause (symbol ",")
  pure $ \range -> Fn range $ NonEmpty.fromList clauses

-- | pClause parses C-style clauses with optional parentheses
-- > clause = "(" pattern ("," pattern)* ")" "->" stmts
-- >        | pattern+ "->" stmts
pClause :: (Features :> es) => Parser es (Clause (Malgo Parse))
pClause = captureRange do
  patterns <-
    try (between (symbol "(") (symbol ")") (sepEndBy pAtomPat (symbol ",")) <* reservedOperator "->")
      <|> try (some pAtomPat <* reservedOperator "->")
      <|> pure []
  body <- pStmts
  pure $ \range -> Clause range patterns body

-- | pAtomPat parses atomic patterns with C-style syntax
pAtomPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pAtomPat =
  choice
    [ pVarP,
      pLiteralP,
      try pTupleP,
      pRecordP,
      pListP
    ]

-- | pTupleP parses C-style tuple patterns with braces
pTupleP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pTupleP = captureRange do
  patterns <- between (symbol "{") (symbol "}") (sepBy pPat (symbol ","))
  case patterns of
    [_] -> fail "c-style tuple must have at least two patterns or be empty"
    _ -> pure $ \range -> TupleP range patterns

-- | pPat parses patterns with C-style syntax
pPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pPat = try pConP <|> pAtomPat

-- | pConP parses constructor patterns with C-style syntax
pConP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pConP = captureRange do
  constructor <- ident
  patterns <- some pAtomPat
  pure $ \range -> ConP range constructor patterns

-- | pStmts parses statements using C-style syntax
pStmts :: (Features :> es) => Parser es (Expr (Malgo Parse))
pStmts = captureRange do
  stmts <- sepEndBy1 pStmt (symbol ";")
  pure $ \range -> Seq range $ NonEmpty.fromList stmts

-- | pStmt parses statements using C-style syntax
pStmt :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pStmt = pLet <|> pWith <|> pNoBind

-- | pLet parses let statements using C-style syntax
pLet :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pLet = captureRange do
  reserved "let"
  name <- ident
  reservedOperator "="
  body <- pExpr
  pure $ \range -> Let range name body

-- | pWith parses with statements using C-style syntax
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

-- | pNoBind parses no-bind statements using C-style syntax
pNoBind :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pNoBind = captureRange do
  body <- pExpr
  pure $ \range -> NoBind range body

-- | pSeq parses parenthesized sequences using C-style syntax
pSeq :: (Features :> es) => Parser es (Expr (Malgo Parse))
pSeq = between (symbol "(") (symbol ")") pStmts

-- | pList parses list literals using C-style syntax
pList :: (Features :> es) => Parser es (Expr (Malgo Parse))
pList = between (symbol "[") (symbol "]") do
  captureRange do
    elements <- sepEndBy pExpr (symbol ",")
    pure $ \range -> List range elements

-- | pRecordP parses record patterns using C-style syntax
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

-- | pListP parses list patterns using C-style syntax
pListP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pListP = captureRange do
  pats <- between (symbol "[") (symbol "]") $ sepEndBy pPat (symbol ",")
  pure $ \range -> ListP range pats
