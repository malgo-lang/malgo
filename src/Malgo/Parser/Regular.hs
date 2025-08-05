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
  runParserT (pRegularModule <* eof) srcPath text

-- | pRegularModule parses a complete module using regular syntax
pRegularModule :: (IOE :> es, Workspace :> es, Features :> es) => Parser es (Module (Malgo Parse))
pRegularModule = do
  space -- consume leading whitespace and comments
  sourcePath <- (.sourceName) <$> getSourcePos
  pwd <- lift pwdPath
  sourcePath <- lift $ parseArtifactPath pwd sourcePath
  decls <- many pRegularDecl
  pure
    Module
      { moduleName = Artifact sourcePath,
        moduleDefinition = ParsedDefinitions decls
      }

-- | pRegularDecl parses declarations with regular expression syntax
pRegularDecl :: (IOE :> es, Workspace :> es, Features :> es) => Parser es (Decl (Malgo Parse))
pRegularDecl = do
  _ <- many skipPragma
  choice
    [ pDataDef,
      pTypeSynonym,
      pInfix,
      pForeign,
      pImport,
      try pScSig, -- must be before pRegularScDef
      pRegularScDef
    ]

-- | pRegularScDef parses value definitions with regular expression syntax
pRegularScDef :: (Features :> es) => Parser es (Decl (Malgo Parse))
pRegularScDef = do
  start <- getSourcePos
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator "="
  body <- pRegularExpr
  end <- getSourcePos
  pure $ ScDef (Range start end) name body

-- | pRegularExpr parses expressions with regular syntax (no C-style)
pRegularExpr :: (Features :> es) => Parser es (Expr (Malgo Parse))
pRegularExpr = do
  start <- getSourcePos
  expr <- pRegularOpApp
  -- try parse a type annotation
  try (pAnn start expr) <|> pure expr
  where
    pAnn start expr = do
      reservedOperator ":"
      ty <- pType
      end <- getSourcePos
      pure $ Ann (Range start end) expr ty

-- | pRegularOpApp parses operator application with regular syntax
pRegularOpApp :: (Features :> es) => Parser es (Expr (Malgo Parse))
pRegularOpApp = makeExprParser pRegularApply table
  where
    table =
      [ [ InfixL do
            start <- getSourcePos
            op <- operator
            end <- getSourcePos
            pure $ OpApp (Range start end) op
        ]
      ]

-- | pRegularApply parses traditional space-separated function application
pRegularApply :: (Features :> es) => Parser es (Expr (Malgo Parse))
pRegularApply =
  makeExprParser
    pRegularProject
    [ [ Postfix $ manyUnaryOp do
          start <- getSourcePos
          argument <- pRegularProject
          end <- getSourcePos
          pure \fn -> Apply (Range start end) fn argument
      ]
    ]

-- | pRegularProject parses field projection
pRegularProject :: (Features :> es) => Parser es (Expr (Malgo Parse))
pRegularProject =
  makeExprParser
    pRegularAtom
    [ [ Postfix $ manyUnaryOp do
          start <- getSourcePos
          reservedOperator "."
          field <- ident
          end <- getSourcePos
          pure \record -> Project (Range start end) record field
      ]
    ]

-- | pRegularAtom parses atoms without C-style syntax
pRegularAtom :: (Features :> es) => Parser es (Expr (Malgo Parse))
pRegularAtom =
  choice
    [ pLiteral,
      pVariable,
      try pRegularTuple,
      try pRegularRecord,
      pRegularFn,
      pList,
      pRegularSeq
    ]

-- | pRegularTuple parses traditional tuple syntax with parentheses
-- > tuple = "(" expr ("," expr)* ")" | "(" ")"
pRegularTuple :: (Features :> es) => Parser es (Expr (Malgo Parse))
pRegularTuple = do
  start <- getSourcePos
  exprs <- between (symbol "(") (symbol ")") (sepBy pRegularExpr (symbol ","))
  end <- getSourcePos
  case exprs of
    [expr] -> pure $ Parens (Range start end) expr
    _ -> pure $ Tuple (Range start end) exprs

-- | pRegularRecord parses record syntax (traditional)
pRegularRecord :: (Features :> es) => Parser es (Expr (Malgo Parse))
pRegularRecord = do
  start <- getSourcePos
  fields <- between (symbol "{") (symbol "}") $ sepEndBy1 pField (symbol ",")
  end <- getSourcePos
  pure $ Record (Range start end) fields
  where
    pField = label "record field" do
      field <- ident
      reservedOperator "="
      value <- pRegularExpr
      pure (field, value)

-- | pRegularFn parses function syntax without C-style clauses
pRegularFn :: (Features :> es) => Parser es (Expr (Malgo Parse))
pRegularFn = do
  start <- getSourcePos
  clauses <- between (symbol "{") (symbol "}") $ sepEndBy1 pRegularClause (symbol ",")
  end <- getSourcePos
  pure $ Fn (Range start end) $ NonEmpty.fromList clauses

-- | pRegularClause parses traditional clauses without parentheses
-- > clause = pattern+ "->" stmts
pRegularClause :: (Features :> es) => Parser es (Clause (Malgo Parse))
pRegularClause = do
  start <- getSourcePos
  patterns <- try (some pRegularAtomPat <* reservedOperator "->") <|> pure []
  body <- pRegularStmts
  end <- getSourcePos
  pure $ Clause (Range start end) patterns body

-- | pRegularAtomPat parses atomic patterns
pRegularAtomPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pRegularAtomPat =
  choice
    [ pVarP,
      pLiteralP,
      try pRegularTupleP,
      pRecordP,
      pListP
    ]

-- | pRegularTupleP parses traditional tuple patterns with parentheses
pRegularTupleP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pRegularTupleP = do
  start <- getSourcePos
  patterns <- between (symbol "(") (symbol ")") (sepBy pRegularPat (symbol ","))
  end <- getSourcePos
  case patterns of
    [pattern] -> pure pattern -- Just return the single pattern, no ParenP wrapper
    _ -> pure $ TupleP (Range start end) patterns

-- | pRegularPat parses patterns
pRegularPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pRegularPat = try pRegularConP <|> pRegularAtomPat

-- | pRegularConP parses constructor patterns
pRegularConP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pRegularConP = do
  start <- getSourcePos
  constructor <- ident
  patterns <- some pRegularAtomPat
  end <- getSourcePos
  pure $ ConP (Range start end) constructor patterns

-- | pRegularStmts parses statements using regular syntax
pRegularStmts :: (Features :> es) => Parser es (Expr (Malgo Parse))
pRegularStmts = do
  start <- getSourcePos
  stmts <- sepEndBy1 pRegularStmt (symbol ";")
  end <- getSourcePos
  pure $ Seq (Range start end) $ NonEmpty.fromList stmts

-- | pRegularStmt parses statements using regular syntax
pRegularStmt :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pRegularStmt = pRegularLet <|> pRegularWith <|> pRegularNoBind

-- | pRegularLet parses let statements using regular syntax
pRegularLet :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pRegularLet = do
  start <- getSourcePos
  reserved "let"
  name <- ident
  reservedOperator "="
  body <- pRegularExpr
  end <- getSourcePos
  pure $ Let (Range start end) name body

-- | pRegularWith parses with statements using regular syntax
pRegularWith :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pRegularWith = do
  start <- getSourcePos
  reserved "with"
  choice
    [ try do
        name <- ident
        reservedOperator "="
        body <- pRegularExpr
        end <- getSourcePos
        pure $ With (Range start end) (Just name) body,
      do
        body <- pRegularExpr
        end <- getSourcePos
        pure $ With (Range start end) Nothing body
    ]

-- | pRegularNoBind parses no-bind statements using regular syntax
pRegularNoBind :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pRegularNoBind = do
  start <- getSourcePos
  body <- pRegularExpr
  end <- getSourcePos
  pure $ NoBind (Range start end) body

-- | pRegularSeq parses parenthesized sequences using regular syntax
pRegularSeq :: (Features :> es) => Parser es (Expr (Malgo Parse))
pRegularSeq = between (symbol "(") (symbol ")") pRegularStmts

-- | pList parses list literals using regular syntax
pList :: (Features :> es) => Parser es (Expr (Malgo Parse))
pList = between (symbol "[") (symbol "]") do
  start <- getSourcePos
  elements <- sepEndBy pRegularExpr (symbol ",")
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
      value <- pRegularPat
      pure (field, value)

-- | pListP parses list patterns using regular syntax
pListP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pListP = do
  start <- getSourcePos
  pats <- between (symbol "[") (symbol "]") $ sepEndBy pRegularPat (symbol ",")
  end <- getSourcePos
  pure $ ListP (Range start end) pats
