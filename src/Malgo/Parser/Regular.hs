module Malgo.Parser.Regular (parseRegular) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.Lazy qualified as TL
import Effectful (Eff, IOE, type (:>))
import Malgo.Features (Features)
import Malgo.Module (ModuleName (..), Workspace, parseArtifactPath, pwdPath)
import Malgo.Parser.Core
  ( Parser,
    captureRange,
    decimal,
    ident,
    lexeme,
    manyUnaryOp,
    operator,
    optional,
    reserved,
    reservedOperator,
    skipPragma,
    space,
    symbol,
  )
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec hiding (optional, parse)
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L

-- * Entry Point

-- | parseRegular parses traditional Malgo syntax without C-style features
parseRegular :: (IOE :> es, Workspace :> es, Features :> es) => FilePath -> TL.Text -> Eff es (Either (ParseErrorBundle TL.Text Void) (Module (Malgo Parse)))
parseRegular srcPath text = do
  runParserT (pModule <* eof) srcPath text

-- * Module and Declaration Parsing

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

-- * Declaration Parsers

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

-- | pScSig parses a value signature.
--
-- > scSig = "def" (ident | "(" operator")")":" type ;
pScSig :: Parser es (Decl (Malgo Parse))
pScSig = captureRange do
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator ":"
  ty <- pType
  pure $ \range -> ScSig range name ty

-- | pScDef parses value definitions with regular expression syntax
pScDef :: (Features :> es) => Parser es (Decl (Malgo Parse))
pScDef = captureRange do
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator "="
  body <- pExpr
  pure $ \range -> ScDef range name body

-- | pInfix parses an infix declaration.
--
-- > infix = "infixl" decimal operator | "infixr" decimal operator | "infix" decimal operator ;
pInfix :: Parser es (Decl (Malgo Parse))
pInfix = captureRange do
  choice
    [ do
        reserved "infixl"
        precedence <- decimal
        operator <- between (symbol "(") (symbol ")") operator
        pure $ \range -> Infix range LeftA precedence operator,
      do
        reserved "infixr"
        precedence <- decimal
        operator <- between (symbol "(") (symbol ")") operator
        pure $ \range -> Infix range RightA precedence operator,
      do
        reserved "infix"
        precedence <- decimal
        operator <- between (symbol "(") (symbol ")") operator
        pure $ \range -> Infix range NeutralA precedence operator
    ]

-- | pForeign parses a foreign declaration.
--
-- > foreign = "foreign" "import" ident ":" type ;
pForeign :: Parser es (Decl (Malgo Parse))
pForeign = captureRange do
  reserved "foreign"
  reserved "import"
  name <- ident
  reservedOperator ":"
  ty <- pType
  pure $ \range -> Foreign range name ty

-- | pImport parses an import declaration.
--
-- > import = "module" importList "=" "import" moduleName
-- > importList = "{" ".." "}"
-- >            | "{" importItem ("," importItem)* "}"
-- >            | moduleName ;
-- > importItem = ident | "(" operator ")" ;
pImport :: (IOE :> es, Workspace :> es) => Parser es (Decl (Malgo Parse))
pImport = captureRange do
  reserved "module"
  importList <- pImportList
  reservedOperator "="
  reserved "import"
  moduleName <- pModuleName
  pure $ \range -> Import range moduleName importList
  where
    pImportList = choice [try pAll, pSelected, pAs]
    pAll = between (symbol "{") (symbol "}") do
      symbol ".."
      pure Malgo.Syntax.Extension.All
    pSelected = between (symbol "{") (symbol "}") do
      items <- sepBy pImportItem (symbol ",")
      pure $ Selected items
    pAs = As <$> pModuleName
    pImportItem = choice [ident, between (symbol "(") (symbol ")") operator]
    pModuleName = asIdent <|> asPath
    asIdent = ModuleName <$> ident
    asPath = do
      path <- pStringLiteral
      sourcePath <- (.sourceName) <$> getSourcePos
      pwd <- lift pwdPath
      sourcePath <- lift $ parseArtifactPath pwd sourcePath
      path' <- lift $ parseArtifactPath sourcePath path
      pure $ Artifact path'

-- * Type Parsers

-- | pType parses a type.
--
-- > type = tyapp "->" type
-- >      | tyapp ;
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

-- | pTyApp parses a type application.
--
-- > tyapp = atomType atomType* ;
pTyApp :: Parser es (Type (Malgo Parse))
pTyApp = captureRange do
  ty <- pAtomType
  tys <- many pAtomType
  pure $ \range -> case tys of
    [] -> ty
    _ -> TyApp range ty tys

-- | pAtomType parses an atomic type.
--
-- > atomType = tyVar
-- >          | tyTuple
-- >          | tyRecord
-- >          | tyBlock
pAtomType :: Parser es (Type (Malgo Parse))
pAtomType = choice [pTyVar, pTyTuple, try pTyRecord, pTyBlock]

-- | pTyVar parses a type variable.
--
-- > tyVar = ident ;
pTyVar :: Parser es (Type (Malgo Parse))
pTyVar = captureRange do
  name <- ident
  pure $ \range -> TyVar range name

-- | pTyTuple parses a tuple type or parenthesized type.
--
-- > tyTuple = "(" type ("," type)* ")"
-- >         | "(" ")" ;
pTyTuple :: Parser es (Type (Malgo Parse))
pTyTuple = captureRange do
  tys <- between (symbol "(") (symbol ")") (sepBy pType (symbol ","))
  pure $ \range -> case tys of
    [ty] -> ty
    _ -> TyTuple range tys

-- | pTyRecord parses a record type.
--
-- > tyRecord = "{" ident "=" type ("," ident "=" type)* "}" ;
pTyRecord :: Parser es (Type (Malgo Parse))
pTyRecord = captureRange do
  fields <- between (symbol "{") (symbol "}") $ sepEndBy1 pField (symbol ",")
  pure $ \range -> TyRecord range fields
  where
    pField = do
      field <- ident
      reservedOperator ":"
      value <- pType
      pure (field, value)

-- | pTyBlock parses a block type.
--
-- > tyBlock = "{" type "}" ;
pTyBlock :: Parser es (Type (Malgo Parse))
pTyBlock = captureRange do
  ty <- between (symbol "{") (symbol "}") pType
  pure $ \range -> TyBlock range ty

-- * Expression Parsers

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

-- | pList parses list literals using regular syntax
pList :: (Features :> es) => Parser es (Expr (Malgo Parse))
pList = between (symbol "[") (symbol "]") do
  captureRange do
    elements <- sepEndBy pExpr (symbol ",")
    pure $ \range -> List range elements

-- | pSeq parses parenthesized sequences using regular syntax
pSeq :: (Features :> es) => Parser es (Expr (Malgo Parse))
pSeq = between (symbol "(") (symbol ")") pStmts

-- * Statement Parsers

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

-- * Pattern Parsers

-- | pPat parses patterns
pPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pPat = try pConP <|> pAtomPat

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

-- | pConP parses constructor patterns
pConP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pConP = captureRange do
  constructor <- ident
  patterns <- some pAtomPat
  pure $ \range -> ConP range constructor patterns

-- | pTupleP parses traditional tuple patterns with parentheses
pTupleP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pTupleP = captureRange do
  patterns <- between (symbol "(") (symbol ")") (sepBy pPat (symbol ","))
  case patterns of
    [pattern] -> pure $ const pattern -- Just return the single pattern, no ParenP wrapper
    _ -> pure $ \range -> TupleP range patterns

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

-- | pVarP parses a variable pattern.
--
-- > varPat = ident ;
pVarP :: Parser es (Pat (Malgo Parse))
pVarP = captureRange do
  name <- ident
  pure $ \range -> VarP range name

-- * Literal Parsers

-- | pLiteral parses a literal.
--
-- > literal = boxed ["#"] ;
-- > boxed = double | float | int | long | char | string ;
pLiteral :: Parser es (Expr (Malgo Parse))
pLiteral = captureRange do
  boxed <- pBoxed
  sharp <- optional (symbol "#")
  pure $ \range -> case sharp of
    Just _ -> Unboxed range $ coerce boxed
    Nothing -> Boxed range boxed

-- | pLiteralP parses a literal pattern.
pLiteralP :: Parser es (Pat (Malgo Parse))
pLiteralP = captureRange do
  boxed <- pBoxed
  sharp <- optional (symbol "#")
  pure $ \range -> case sharp of
    Just _ -> UnboxedP range $ coerce boxed
    Nothing -> BoxedP range boxed

pBoxed :: Parser es (Literal Boxed)
pBoxed = choice [try pReal, pInt, pChar, pString]

-- | pReal parses a real number.
--
-- > real = double | float ;
-- > double = FLOAT ;
-- > float = FLOAT "f" | FLOAT "F" ;
pReal :: Parser es (Literal Boxed)
pReal = lexeme do
  f <- L.float
  tail <- optional (char 'f' <|> char 'F')
  case tail of
    Just _ -> pure $ Float (realToFrac f)
    Nothing -> pure $ Double f

-- | pInt parses an integer.
--
-- > int = int32| int64 ;
-- > int32 = DECIMAL ;
-- > int64 = DECIMAL "l" | DECIMAL "L" ;
pInt :: Parser es (Literal Boxed)
pInt = lexeme do
  i <- L.decimal
  tail <- optional (char 'l' <|> char 'L')
  case tail of
    Just _ -> pure $ Int64 i
    Nothing -> pure $ Int32 (fromIntegral i)

-- | pChar parses a character.
--
-- > char = "'" CHAR "'" ;
pChar :: Parser es (Literal Boxed)
pChar = lexeme do
  void $ char '\''
  c <- L.charLiteral
  void $ char '\''
  pure $ Char c

-- | pString parses a string.
--
-- > string = "\"" CHAR* "\"" ;
pString :: Parser es (Literal Boxed)
pString = String <$> pStringLiteral

pStringLiteral :: (ConvertibleStrings String a) => Parser es a
pStringLiteral = lexeme do
  void $ char '"'
  str <- manyTill L.charLiteral (char '"')
  pure $ convertString str

-- * Utility Functions

-- | pVariable parses a variable.
--
-- > variable = ident ;
pVariable :: Parser es (Expr (Malgo Parse))
pVariable = captureRange do
  name <- ident
  pure $ \range -> Var range name

-- | pClause parses traditional clauses without parentheses
-- > clause = pattern+ "->" stmts
pClause :: (Features :> es) => Parser es (Clause (Malgo Parse))
pClause = captureRange do
  patterns <- try (some pAtomPat <* reservedOperator "->") <|> pure []
  body <- pStmts
  pure $ \range -> Clause range patterns body
