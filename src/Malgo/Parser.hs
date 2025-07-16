{-# LANGUAGE NoMonomorphismRestriction #-}

module Malgo.Parser (parse, ParserPass (..)) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Text.Lazy qualified as TL
import Effectful
import Effectful.Error.Static (throwError)
import Effectful.FileSystem (runFileSystem)
import Malgo.Features
import Malgo.Module (ModuleName (..), Workspace, parseArtifactPath, pwdPath)
import Malgo.Pass
import Malgo.Prelude hiding (All)
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec hiding (optional, parse)
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L

data ParserPass = ParserPass

instance Pass ParserPass where
  type Input ParserPass = (FilePath, TL.Text)
  type Output ParserPass = Module (Malgo Parse)
  type ErrorType ParserPass = ParseErrorBundle TL.Text Void
  type Effects ParserPass es = (IOE :> es, Workspace :> es, Features :> es)

  runPassImpl _ (srcPath, text) = do
    result <- parse srcPath text
    case result of
      Left err -> throwError err
      Right result -> pure result

type Parser es = ParsecT Void TL.Text (Eff es)

-- | parse a module. Returns the list of pragmas and the module.
parse :: (IOE :> es, Workspace :> es, Features :> es) => FilePath -> TL.Text -> Eff es (Either (ParseErrorBundle TL.Text Void) (Module (Malgo Parse)))
parse srcPath text = runFileSystem do
  let features = parseFeatures $ extractPragmas text
  -- Enable CStyleApply by default for brace tuple syntax
  addFeatures (features <> FeatureFlags (Set.singleton CStyleApply))
  runParserT parser srcPath text

-- | Extract pragmas from a module.
-- Returns the list of pragmas.
extractPragmas :: TL.Text -> [Text]
extractPragmas = go [] . TL.lines
  where
    go pragmas [] = map convertString $ reverse pragmas
    go pragmas (l : ls)
      | "#" `TL.isPrefixOf` l = go (TL.drop 1 l : pragmas) ls
      | otherwise = go pragmas ls

parser :: (IOE :> es, Workspace :> es, Features :> es) => Parser es (Module (Malgo Parse))
parser = do
  space
  mod <- pModuleFile
  eof
  pure mod

pModuleFile :: (IOE :> es, Workspace :> es, Features :> es) => Parser es (Module (Malgo Parse))
pModuleFile = do
  sourcePath <- (.sourceName) <$> getSourcePos
  pwd <- lift pwdPath
  sourcePath <- lift $ parseArtifactPath pwd sourcePath
  decls <- many pDecl
  pure
    Module
      { moduleName = Artifact sourcePath,
        moduleDefinition = ParsedDefinitions decls
      }

-- * Declarations

-- | pDecl parses a declaration and skips any leading pragmas.
--
-- > decl = dataDef | typeSynonym | infix | foreign | import | scSig | scDef ;
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

-- | skipPragma skips a pragma.
--
-- > pragma = "#" any* "\n" ;
skipPragma :: Parser es ()
skipPragma = lexeme do
  void $ char '#'
  void $ takeWhileP (Just "pragma") (/= '\n')

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

-- | pInfix parses an infix declaration.
--
-- > infix = "infixl" decimal operator | "infixr" decimal operator | "infix" decimal operator ;
pInfix :: Parser es (Decl (Malgo Parse))
pInfix = do
  start <- getSourcePos
  choice
    [ do
        reserved "infixl"
        precedence <- decimal
        operator <- between (symbol "(") (symbol ")") operator
        end <- getSourcePos
        pure $ Infix (Range start end) LeftA precedence operator,
      do
        reserved "infixr"
        precedence <- decimal
        operator <- between (symbol "(") (symbol ")") operator
        end <- getSourcePos
        pure $ Infix (Range start end) RightA precedence operator,
      do
        reserved "infix"
        precedence <- decimal
        operator <- between (symbol "(") (symbol ")") operator
        end <- getSourcePos
        pure $ Infix (Range start end) NeutralA precedence operator
    ]

-- | pForeign parses a foreign declaration.
--
-- > foreign = "foreign" "import" ident ":" type ;
pForeign :: Parser es (Decl (Malgo Parse))
pForeign = do
  start <- getSourcePos
  reserved "foreign"
  reserved "import"
  name <- ident
  reservedOperator ":"
  ty <- pType
  end <- getSourcePos
  pure $ Foreign (Range start end) name ty

-- | pImport parses an import declaration.
--
-- > import = "module" importList "=" "import" moduleName
-- > importList = "{" ".." "}"
-- >            | "{" importItem ("," importItem)* "}"
-- >            | moduleName ;
-- > importItem = ident | "(" operator ")" ;
pImport :: (IOE :> es, Workspace :> es) => Parser es (Decl (Malgo Parse))
pImport = do
  start <- getSourcePos
  reserved "module"
  importList <- pImportList
  reservedOperator "="
  reserved "import"
  moduleName <- pModuleName
  end <- getSourcePos
  pure $ Import (Range start end) moduleName importList
  where
    pImportList = choice [try pAll, pSelected, pAs]
    pAll = between (symbol "{") (symbol "}") do
      symbol ".."
      pure All
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

-- | pScSig parses a value signature.
--
-- > scSig = "def" (ident | "(" operator")")":" type ;
pScSig :: Parser es (Decl (Malgo Parse))
pScSig = do
  start <- getSourcePos
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator ":"
  ty <- pType
  end <- getSourcePos
  pure $ ScSig (Range start end) name ty

-- | pScDef parses a value definition.
--
-- > scDef = "def" (ident | "(" operator")") "=" expr ;
pScDef :: (Features :> es) => Parser es (Decl (Malgo Parse))
pScDef = do
  start <- getSourcePos
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator "="
  body <- pExpr
  end <- getSourcePos
  pure $ ScDef (Range start end) name body

-- * Exprs

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

-- | pOpApp parses an operator application.
--
-- > opApp = apply (operator apply)* ;
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

-- | pApply parses an application.
--
-- > apply = project project* ;
-- if c-style-apply is enabled:
-- > apply = project
-- >       | project "(" ")"
-- >       | project "(" expr ("," expr)* ")" ;
pApply :: (Features :> es) => Parser es (Expr (Malgo Parse))
pApply = do
  cStyleApply <- lift $ hasFeature CStyleApply
  if cStyleApply
    then pCStyleApply
    else
      makeExprParser
        pProject
        [ [ Postfix $ manyUnaryOp do
              start <- getSourcePos
              argument <- pProject
              end <- getSourcePos
              pure \fn -> Apply (Range start end) fn argument
          ]
        ]
  where
    pCStyleApply = do
      makeExprParser
        pProject
        [ [ Postfix $ manyUnaryOp do
              start <- getSourcePos
              args <- between (symbol "(") (symbol ")") (sepBy pExpr (symbol ","))
              end <- getSourcePos
              pure \fn -> foldl (Apply (Range start end)) fn args
          ]
        ]

-- | pProject parses a projection.
--
-- > project = atom ("." ident)* ;
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

-- | pAtom parses an atom.
--
-- > atom = literal
-- >      | variable
-- >      | tuple
-- >      | brace
-- >      | list
-- >      | seq
pAtom ::
  (Features :> es) => Parser es (Expr (Malgo Parse))
pAtom =
  choice
    [ pLiteral,
      pVariable,
      try pTuple,
      try pRecord,
      pFn,
      pList,
      pSeq,
      pParens
    ]

-- | pLiteral parses a literal.
--
-- > literal = boxed ["#"] ;
-- > boxed = double | float | int | long | char | string ;
pLiteral :: Parser es (Expr (Malgo Parse))
pLiteral = do
  start <- getSourcePos
  boxed <- pBoxed
  sharp <- optional (symbol "#")
  end <- getSourcePos
  case sharp of
    Just _ -> pure $ Unboxed (Range start end) $ coerce boxed
    Nothing -> pure $ Boxed (Range start end) boxed

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

-- | pVariable parses a variable.
--
-- > variable = ident ;
pVariable :: Parser es (Expr (Malgo Parse))
pVariable = do
  start <- getSourcePos
  name <- ident
  end <- getSourcePos
  pure $ Var (Range start end) name

-- | pTuple parses a tuple or parenthesized expression.
--
-- > tuple = "{" expr ("," expr)* "}"
-- >       | "{" "}"
pTuple :: (Features :> es) => Parser es (Expr (Malgo Parse))
pTuple = do
  start <- getSourcePos
  exprs <- between (symbol "{") (symbol "}") (sepBy pExpr (symbol ","))
  end <- getSourcePos
  case exprs of
    [_] -> fail "tuple must have at least two expressions or be empty"
    _ -> pure $ Tuple (Range start end) exprs

-- > record = ident "=" expr ("," ident "=" expr)* ;
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

-- > function = clause ("," clause)* ;
pFn :: (Features :> es) => Parser es (Expr (Malgo Parse))
pFn = do
  start <- getSourcePos
  clauses <- between (symbol "{") (symbol "}") $ sepEndBy1 pClause (symbol ",")
  end <- getSourcePos
  pure $ Fn (Range start end) $ NonEmpty.fromList clauses

-- > clause = pattern+ "->" stmts ;
pClause :: (Features :> es) => Parser es (Clause (Malgo Parse))
pClause = do
  start <- getSourcePos
  cStyleApply <- lift $ hasFeature CStyleApply
  patterns <-
    if cStyleApply
      then do
        let withParens = between (symbol "(") (symbol ")") $ sepEndBy pAtomPat (symbol ",")
        let withoutParens = sepEndBy pAtomPat (symbol ",")
        try (withParens <* reservedOperator "->") <|> try (withoutParens <* reservedOperator "->") <|> pure []
      else try (some pAtomPat <* reservedOperator "->") <|> pure []
  body <- pStmts
  end <- getSourcePos
  pure $ Clause (Range start end) patterns body

pStmts :: (Features :> es) => Parser es (Expr (Malgo Parse))
pStmts = do
  start <- getSourcePos
  stmts <- sepEndBy1 pStmt (symbol ";")
  end <- getSourcePos
  pure $ Seq (Range start end) $ NonEmpty.fromList stmts

pStmt :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pStmt = pLet <|> pWith <|> pNoBind

pLet :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pLet = do
  start <- getSourcePos
  reserved "let"
  name <- ident
  reservedOperator "="
  body <- pExpr
  end <- getSourcePos
  pure $ Let (Range start end) name body

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

pNoBind :: (Features :> es) => Parser es (Stmt (Malgo Parse))
pNoBind = do
  start <- getSourcePos
  body <- pExpr
  end <- getSourcePos
  pure $ NoBind (Range start end) body

pList :: (Features :> es) => Parser es (Expr (Malgo Parse))
pList = between (symbol "[") (symbol "]") do
  start <- getSourcePos
  elements <- sepEndBy pExpr (symbol ",")
  end <- getSourcePos
  pure $ List (Range start end) elements

pSeq :: (Features :> es) => Parser es (Expr (Malgo Parse))
pSeq = between (symbol "(") (symbol ")") pStmts

-- * Patterns

-- | pPat parses a pattern.
--
-- > pattern = ident atomPat+
-- >         | atomPat ;
pPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pPat = try pConP <|> pAtomPat

pConP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pConP = do
  start <- getSourcePos
  name <- ident
  pats <- some pAtomPat
  end <- getSourcePos
  pure $ ConP (Range start end) name pats

-- | pAtomPat parses an atomic pattern.
--
-- > atomPat = variable
-- >         | literal
-- >         | tuplePat
-- >         | recordPat
-- >         | listPat
pAtomPat :: (Features :> es) => Parser es (Pat (Malgo Parse))
pAtomPat =
  choice
    [ pVarP,
      pLiteralP,
      try pTupleP,
      pRecordP,
      pListP
    ]

-- | pVarP parses a variable pattern.
--
-- > varPat = ident ;
pVarP :: Parser es (Pat (Malgo Parse))
pVarP = do
  start <- getSourcePos
  name <- ident
  end <- getSourcePos
  pure $ VarP (Range start end) name

-- | pLiteralP parses a literal pattern.
pLiteralP :: Parser es (Pat (Malgo Parse))
pLiteralP = do
  start <- getSourcePos
  boxed <- pBoxed
  sharp <- optional (symbol "#")
  end <- getSourcePos
  case sharp of
    Just _ -> pure $ UnboxedP (Range start end) $ coerce boxed
    Nothing -> pure $ BoxedP (Range start end) boxed

-- | pTupleP parses a tuple pattern or parenthesized pattern.
--
-- > tuplePat = "{" pattern ("," pattern)* "}"
-- >          | "{" "}" ;
pTupleP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pTupleP = do
  start <- getSourcePos
  pats <- between (symbol "{") (symbol "}") (sepBy pPat (symbol ","))
  end <- getSourcePos
  case pats of
    [_] -> fail "tuple must have at least two patterns or be empty"
    _ -> pure $ TupleP (Range start end) pats

-- | pRecordP parses a record pattern.
--
-- > recordPat = "{" ident "=" pattern ("," ident "=" pattern)* "}" ;
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

-- | pListP parses a list pattern.
--
-- > listPat = "[" pattern ("," pattern)* "]" ;
pListP :: (Features :> es) => Parser es (Pat (Malgo Parse))
pListP = do
  start <- getSourcePos
  pats <- between (symbol "[") (symbol "]") $ sepEndBy pPat (symbol ",")
  end <- getSourcePos
  pure $ ListP (Range start end) pats

-- * Types

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
pTyApp = do
  start <- getSourcePos
  ty <- pAtomType
  tys <- many pAtomType
  end <- getSourcePos
  case tys of
    [] -> pure ty
    _ -> pure $ TyApp (Range start end) ty tys

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
pTyVar = do
  start <- getSourcePos
  name <- ident
  end <- getSourcePos
  pure $ TyVar (Range start end) name

-- | pTyTuple parses a tuple type or parenthesized type.
--
-- > tyTuple = "(" type ("," type)* ")"
-- >         | "(" ")" ;
pTyTuple :: Parser es (Type (Malgo Parse))
pTyTuple = do
  start <- getSourcePos
  tys <- between (symbol "(") (symbol ")") (sepBy pType (symbol ","))
  end <- getSourcePos
  case tys of
    [ty] -> pure ty
    _ -> pure $ TyTuple (Range start end) tys

-- | pTyRecord parses a record type.
--
-- > tyRecord = "{" ident "=" type ("," ident "=" type)* "}" ;
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

-- | pTyBlock parses a block type.
--
-- > tyBlock = "{" type "}" ;
pTyBlock :: Parser es (Type (Malgo Parse))
pTyBlock = do
  start <- getSourcePos
  ty <- between (symbol "{") (symbol "}") pType
  end <- getSourcePos
  pure $ TyBlock (Range start end) ty

-- * combinators

-- | optional tries to parse the given parser and returns `Nothing` if it fails.
optional :: Parser es a -> Parser es (Maybe a)
optional p = try (fmap Just p) <|> pure Nothing

-- | manyUnaryOp parses zero or more unary operators and returns a function that applies them in order.
manyUnaryOp :: (MonadPlus f) => f (c -> c) -> f (c -> c)
manyUnaryOp singleUnaryOp = foldr1 (>>>) <$> some singleUnaryOp

-- | space skips zero or more white space characters and comments.
space :: Parser es ()
space = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | lexeme consumes the given parser and skips trailing white space.
lexeme :: Parser es a -> Parser es a
lexeme = L.lexeme space

-- | decimal consumes an integer.
decimal :: Parser es Int
decimal = lexeme L.decimal

-- | symbol consumes the given string.
symbol :: TL.Text -> Parser es ()
symbol = void . L.symbol space

-- | ident consumes an identifier.
ident :: Parser es Text
ident = lexeme do
  notFollowedBy anyReserved
  TL.toStrict . TL.pack <$> ((:) <$> identStart <*> many identContinue)

-- TODO: use XID_Start
identStart :: Parser es Char
identStart = letterChar <|> char '_'

-- TODO: use XID_Continue
identContinue :: Parser es Char
identContinue = alphaNumChar <|> char '_' <|> char '#'

reserved :: TL.Text -> Parser es ()
reserved w
  | w `elem` reservedKeywords = void $ lexeme (string w <* notFollowedBy identContinue)
  | otherwise = fail $ "reserved keyword: " <> show w

anyReserved :: Parser es ()
anyReserved = choice $ map (try . reserved) reservedKeywords

reservedKeywords :: [TL.Text]
reservedKeywords =
  [ "class",
    "def",
    "data",
    "exists",
    "forall",
    "foreign",
    "impl",
    "import",
    "infix",
    "infixl",
    "infixr",
    "let",
    "type",
    "module",
    "with"
  ]

operator :: Parser es Text
operator = lexeme do
  notFollowedBy anyReservedOperator
  convertString <$> some operatorChar

operatorChar :: Parser es Char
operatorChar = oneOf ("+-*/\\%=><:;|&!#." :: String)

reservedOperator :: TL.Text -> Parser es ()
reservedOperator w
  | w `elem` reservedOperators = void $ lexeme (string w <* notFollowedBy operatorChar)
  | otherwise = fail $ "reserved symbol: " <> show w

anyReservedOperator :: Parser es ()
anyReservedOperator = choice $ map (try . reservedOperator) reservedOperators

reservedOperators :: [TL.Text]
reservedOperators = ["=>", "=", ":", "|", "->", ";", ".", ",", "!", "#|", "|#"]
