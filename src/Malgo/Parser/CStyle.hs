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
  runParserT (pCStyleModule <* eof) srcPath text

-- | pCStyleModule parses a complete module using C-style syntax
pCStyleModule :: (IOE :> es, Workspace :> es, Features :> es) => Parser es (Module (Malgo Parse))
pCStyleModule = do
  space -- consume leading whitespace and comments
  sourcePath <- (.sourceName) <$> getSourcePos
  pwd <- lift pwdPath
  sourcePath <- lift $ parseArtifactPath pwd sourcePath
  decls <- many pCStyleDecl
  pure
    Module
      { moduleName = Artifact sourcePath,
        moduleDefinition = ParsedDefinitions decls
      }

-- | pCStyleDecl parses declarations with C-style expression syntax
pCStyleDecl :: (IOE :> es, Workspace :> es, Features :> es) => Parser es (Decl (Malgo Parse))
pCStyleDecl = do
  _ <- many skipPragma
  choice
    [ pDataDef,
      pTypeSynonym,
      pInfix,
      pForeign,
      pImport,
      try pScSig, -- must be before pCStyleScDef
      pCStyleScDef
    ]

-- | pCStyleScDef parses value definitions with C-style expression syntax
pCStyleScDef :: (Features :> es) => Parser es (Decl (Malgo Parse))
pCStyleScDef = do
  start <- getSourcePos
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator "="
  body <- pCStyleExpr
  end <- getSourcePos
  pure $ ScDef (Range start end) name body

-- | pCStyleExpr parses expressions with C-style syntax
pCStyleExpr :: (Features :> es) => Parser es (Expr (Malgo Parse))
pCStyleExpr = do
  start <- getSourcePos
  expr <- pCStyleOpApp
  -- try parse a type annotation
  try (pAnn start expr) <|> pure expr
  where
    pAnn start expr = do
      reservedOperator ":"
      ty <- pType
      end <- getSourcePos
      pure $ Ann (Range start end) expr ty

-- | pCStyleOpApp parses operator application with C-style syntax
pCStyleOpApp :: (Features :> es) => Parser es (Expr (Malgo Parse))
pCStyleOpApp = makeExprParser pCStyleApply table
  where
    table =
      [ [ InfixL do
            start <- getSourcePos
            op <- operator
            end <- getSourcePos
            pure $ OpApp (Range start end) op
        ]
      ]

-- | pCStyleApply parses C-style function application with parentheses
pCStyleApply :: (Features :> es) => Parser es (Expr (Malgo Parse))
pCStyleApply =
  makeExprParser
    pCStyleProject
    [ [ Postfix $ manyUnaryOp do
          start <- getSourcePos
          args <- between (symbol "(") (symbol ")") (sepBy pCStyleExpr (symbol ","))
          end <- getSourcePos
          pure \fn -> foldl (Apply (Range start end)) fn args
      ]
    ]

-- | pCStyleProject parses field projection
pCStyleProject :: (Features :> es) => Parser es (Expr (Malgo Parse))
pCStyleProject =
  makeExprParser
    pCStyleAtom
    [ [ Postfix $ manyUnaryOp do
          start <- getSourcePos
          reservedOperator "."
          field <- ident
          end <- getSourcePos
          pure \record -> Project (Range start end) record field
      ]
    ]

-- | pCStyleAtom parses atoms with C-style syntax
pCStyleAtom :: (Features :> es) => Parser es (Expr (Malgo Parse))
pCStyleAtom =
  choice
    [ pLiteral,
      pVariable,
      try pCStyleTuple,
      try pCStyleRecord,
      pCStyleFn,
      pList,
      pCStyleSeq
    ]

-- | pCStyleTuple parses C-style tuple syntax with braces
-- > tuple = "{" expr ("," expr)+ "}" | "{" "}"
pCStyleTuple :: (Features :> es) => Parser es (Expr (Malgo Parse))
pCStyleTuple = do
  start <- getSourcePos
  exprs <- between (symbol "{") (symbol "}") (sepBy pCStyleExpr (symbol ","))
  end <- getSourcePos
  case exprs of
    [_] -> fail "c-style tuple must have at least two expressions or be empty"
    _ -> pure $ Tuple (Range start end) exprs

-- | pCStyleRecord parses record syntax (separate from tuples in C-style)
pCStyleRecord :: (Features :> es) => Parser es (Expr (Malgo Parse))
pCStyleRecord = do
  start <- getSourcePos
  fields <- between (symbol "{") (symbol "}") $ sepEndBy1 pField (symbol ",")
  end <- getSourcePos
  pure $ Record (Range start end) fields
  where
    pField = label "record field" do
      field <- ident
      reservedOperator "="
      value <- pCStyleExpr
      pure (field, value)

-- | pCStyleFn parses function syntax with C-style clauses
pCStyleFn :: (Features :> es) => Parser es (Expr (Malgo Parse))
pCStyleFn = do
  start <- getSourcePos
  clauses <- between (symbol "{") (symbol "}") $ sepEndBy1 pCStyleClause (symbol ",")
  end <- getSourcePos
  pure $ Fn (Range start end) $ NonEmpty.fromList clauses

-- | pCStyleClause parses C-style clauses with optional parentheses
-- > clause = "(" pattern ("," pattern)* ")" "->" stmts
-- >        | pattern+ "->" stmts
pCStyleClause :: (Features :> es) => Parser es (Clause (Malgo Parse))
pCStyleClause = do
  start <- getSourcePos
  patterns <-
    try (between (symbol "(") (symbol ")") (sepEndBy pCStyleAtomPat (symbol ",")) <* reservedOperator "->")
      <|> try (some pCStyleAtomPat <* reservedOperator "->")
      <|> pure []
  body <- pCStyleStmts
  end <- getSourcePos
  pure $ Clause (Range start end) patterns body

-- | pCStyleAtomPat parses atomic patterns with C-style syntax
pCStyleAtomPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pCStyleAtomPat =
  choice
    [ pVarP,
      pLiteralP,
      try pCStyleTupleP,
      pRecordP,
      pListP
    ]

-- | pCStyleTupleP parses C-style tuple patterns with braces
pCStyleTupleP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pCStyleTupleP = do
  start <- getSourcePos
  patterns <- between (symbol "{") (symbol "}") (sepBy pCStylePat (symbol ","))
  end <- getSourcePos
  case patterns of
    [_] -> fail "c-style tuple must have at least two patterns or be empty"
    _ -> pure $ TupleP (Range start end) patterns

-- | pCStylePat parses patterns with C-style syntax
pCStylePat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pCStylePat = try pCStyleConP <|> pCStyleAtomPat

-- | pCStyleConP parses constructor patterns with C-style syntax
pCStyleConP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pCStyleConP = do
  start <- getSourcePos
  constructor <- ident
  patterns <- some pCStyleAtomPat
  end <- getSourcePos
  pure $ ConP (Range start end) constructor patterns

-- | pCStyleStmts parses statements using C-style syntax
pCStyleStmts :: (Features :> es) => Parser es (Expr (Malgo Parse))
pCStyleStmts = do
  start <- getSourcePos
  stmts <- sepEndBy1 pCStyleStmt (symbol ";")
  end <- getSourcePos
  pure $ Seq (Range start end) $ NonEmpty.fromList stmts

-- | pCStyleStmt parses statements using C-style syntax
pCStyleStmt :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pCStyleStmt = pCStyleLet <|> pCStyleWith <|> pCStyleNoBind

-- | pCStyleLet parses let statements using C-style syntax
pCStyleLet :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pCStyleLet = do
  start <- getSourcePos
  reserved "let"
  name <- ident
  reservedOperator "="
  body <- pCStyleExpr
  end <- getSourcePos
  pure $ Let (Range start end) name body

-- | pCStyleWith parses with statements using C-style syntax
pCStyleWith :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pCStyleWith = do
  start <- getSourcePos
  reserved "with"
  choice
    [ try do
        name <- ident
        reservedOperator "="
        body <- pCStyleExpr
        end <- getSourcePos
        pure $ With (Range start end) (Just name) body,
      do
        body <- pCStyleExpr
        end <- getSourcePos
        pure $ With (Range start end) Nothing body
    ]

-- | pCStyleNoBind parses no-bind statements using C-style syntax
pCStyleNoBind :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pCStyleNoBind = do
  start <- getSourcePos
  body <- pCStyleExpr
  end <- getSourcePos
  pure $ NoBind (Range start end) body

-- | pCStyleSeq parses parenthesized sequences using C-style syntax
pCStyleSeq :: (Features :> es) => Parser es (Expr (Malgo Parse))
pCStyleSeq = between (symbol "(") (symbol ")") pCStyleStmts

-- | pList parses list literals using C-style syntax
pList :: (Features :> es) => Parser es (Expr (Malgo Parse))
pList = between (symbol "[") (symbol "]") do
  start <- getSourcePos
  elements <- sepEndBy pCStyleExpr (symbol ",")
  end <- getSourcePos
  pure $ List (Range start end) elements

-- | pRecordP parses record patterns using C-style syntax
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
      value <- pCStylePat
      pure (field, value)

-- | pListP parses list patterns using C-style syntax
pListP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pListP = do
  start <- getSourcePos
  pats <- between (symbol "[") (symbol "]") $ sepEndBy pCStylePat (symbol ",")
  end <- getSourcePos
  pure $ ListP (Range start end) pats
