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
pDataDef = do
  start <- getSourcePos
  reserved "data"
  name <- ident
  parameters <- many do
    start <- getSourcePos
    parameter <- ident
    end <- getSourcePos
    pure (Range start end, parameter)
  reservedOperator "="
  constructors <- sepBy1 pConstructor (reservedOperator "|")
  end <- getSourcePos
  pure $ DataDef (Range start end) name parameters constructors
  where
    pConstructor = do
      start <- getSourcePos
      name <- ident
      parameters <- many pAtomType
      end <- getSourcePos
      pure (Range start end, name, parameters)

-- | pTypeSynonym parses a type synonym.
--
-- > typeSynonym = "type" ident ident* "=" type ;
pTypeSynonym :: Parser es (Decl (Malgo Parse))
pTypeSynonym = do
  start <- getSourcePos
  reserved "type"
  name <- ident
  parameters <- many ident
  reservedOperator "="
  ty <- pType
  end <- getSourcePos
  pure $ TypeSynonym (Range start end) name parameters ty

-- | pScDef parses value definitions with regular expression syntax
pScDef :: (Features :> es) => Parser es (Decl (Malgo Parse))
pScDef = do
  start <- getSourcePos
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator "="
  body <- pExpr
  end <- getSourcePos
  pure $ ScDef (Range start end) name body

-- | pExpr parses expressions with regular syntax (no C-style)
pExpr :: (Features :> es) => Parser es (Expr (Malgo Parse))
pExpr = do
  start <- getSourcePos
  expr <- pOpApp
  -- try parse a type annotation
  try (pAnn start expr) <|> pure expr
  where
    pAnn start expr = do
      reservedOperator ":"
      ty <- pType
      end <- getSourcePos
      pure $ Ann (Range start end) expr ty

-- | pOpApp parses operator application with regular syntax
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

-- | pApply parses traditional space-separated function application
pApply :: (Features :> es) => Parser es (Expr (Malgo Parse))
pApply =
  makeExprParser
    pProject
    [ [ Postfix $ manyUnaryOp do
          start <- getSourcePos
          argument <- pProject
          end <- getSourcePos
          pure \fn -> Apply (Range start end) fn argument
      ]
    ]

-- | pProject parses field projection
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
pTuple = do
  start <- getSourcePos
  exprs <- between (symbol "(") (symbol ")") (sepBy pExpr (symbol ","))
  end <- getSourcePos
  case exprs of
    [expr] -> pure $ Parens (Range start end) expr
    _ -> pure $ Tuple (Range start end) exprs

-- | pRecord parses record syntax (traditional)
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

-- | pFn parses function syntax without C-style clauses
pFn :: (Features :> es) => Parser es (Expr (Malgo Parse))
pFn = do
  start <- getSourcePos
  clauses <- between (symbol "{") (symbol "}") $ sepEndBy1 pClause (symbol ",")
  end <- getSourcePos
  pure $ Fn (Range start end) $ NonEmpty.fromList clauses

-- | pClause parses traditional clauses without parentheses
-- > clause = pattern+ "->" stmts
pClause :: (Features :> es) => Parser es (Clause (Malgo Parse))
pClause = do
  start <- getSourcePos
  patterns <- try (some pAtomPat <* reservedOperator "->") <|> pure []
  body <- pStmts
  end <- getSourcePos
  pure $ Clause (Range start end) patterns body

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
pTupleP = do
  start <- getSourcePos
  patterns <- between (symbol "(") (symbol ")") (sepBy pPat (symbol ","))
  end <- getSourcePos
  case patterns of
    [pattern] -> pure pattern -- Just return the single pattern, no ParenP wrapper
    _ -> pure $ TupleP (Range start end) patterns

-- | pPat parses patterns
pPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pPat = try pConP <|> pAtomPat

-- | pConP parses constructor patterns
pConP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pConP = do
  start <- getSourcePos
  constructor <- ident
  patterns <- some pAtomPat
  end <- getSourcePos
  pure $ ConP (Range start end) constructor patterns

-- | pStmts parses statements using regular syntax
pStmts :: (Features :> es) => Parser es (Expr (Malgo Parse))
pStmts = do
  start <- getSourcePos
  stmts <- sepEndBy1 pStmt (symbol ";")
  end <- getSourcePos
  pure $ Seq (Range start end) $ NonEmpty.fromList stmts

-- | pStmt parses statements using regular syntax
pStmt :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pStmt = pLet <|> pWith <|> pNoBind

-- | pLet parses let statements using regular syntax
pLet :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pLet = do
  start <- getSourcePos
  reserved "let"
  name <- ident
  reservedOperator "="
  body <- pExpr
  end <- getSourcePos
  pure $ Let (Range start end) name body

-- | pWith parses with statements using regular syntax
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

-- | pNoBind parses no-bind statements using regular syntax
pNoBind :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pNoBind = do
  start <- getSourcePos
  body <- pExpr
  end <- getSourcePos
  pure $ NoBind (Range start end) body

-- | pSeq parses parenthesized sequences using regular syntax
pSeq :: (Features :> es) => Parser es (Expr (Malgo Parse))
pSeq = between (symbol "(") (symbol ")") pStmts

-- | pList parses list literals using regular syntax
pList :: (Features :> es) => Parser es (Expr (Malgo Parse))
pList = between (symbol "[") (symbol "]") do
  start <- getSourcePos
  elements <- sepEndBy pExpr (symbol ",")
  end <- getSourcePos
  pure $ List (Range start end) elements

-- | pRecordP parses record patterns using regular syntax
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

-- | pListP parses list patterns using regular syntax
pListP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pListP = do
  start <- getSourcePos
  pats <- between (symbol "[") (symbol "]") $ sepEndBy pPat (symbol ",")
  end <- getSourcePos
  pure $ ListP (Range start end) pats
