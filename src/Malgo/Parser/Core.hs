{-# HLINT ignore "Use <$>" #-}

module Malgo.Parser.Core
  ( -- * Parser Type and Effects
    Parser,

    -- * Common Parsing Functions
    skipPragma,
    pInfix,
    pForeign,
    pImport,
    pScSig,

    -- * Type Parsing
    pType,
    pTyApp,
    pAtomType,
    pTyVar,
    pTyTuple,
    pTyRecord,
    pTyBlock,

    -- * Literal Parsing
    pLiteral,
    pBoxed,
    pReal,
    pInt,
    pChar,
    pString,
    pStringLiteral,
    pVariable,

    -- * Pattern Parsing (non-feature-specific)
    pVarP,
    pLiteralP,

    -- * Lexical Analysis
    space,
    lexeme,
    decimal,
    symbol,
    ident,
    identStart,
    identContinue,
    reserved,
    anyReserved,
    reservedKeywords,
    operator,
    operatorChar,
    reservedOperator,
    anyReservedOperator,
    reservedOperators,

    -- * Combinators
    optional,
    manyUnaryOp,
    captureRange,
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Trans (lift)
import Data.Text.Lazy qualified as TL
import Effectful
import Malgo.Module (ModuleName (..), Workspace, parseArtifactPath, pwdPath)
import Malgo.Prelude hiding (All)
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec hiding (optional, parse)
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L

-- | Parser type used across all parser modules
type Parser es = ParsecT Void TL.Text (Eff es)

-- | skipPragma skips a pragma.
--
-- > pragma = "#" any* "\n" ;
skipPragma :: Parser es ()
skipPragma = lexeme do
  void $ char '#'
  void $ takeWhileP (Just "pragma") (/= '\n')

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
pScSig = captureRange do
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator ":"
  ty <- pType
  pure $ \range -> ScSig range name ty

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

-- * Literals

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
pVariable = captureRange do
  name <- ident
  pure $ \range -> Var range name

-- * Patterns

-- | pVarP parses a variable pattern.
--
-- > varPat = ident ;
pVarP :: Parser es (Pat (Malgo Parse))
pVarP = captureRange do
  name <- ident
  pure $ \range -> VarP range name

-- | pLiteralP parses a literal pattern.
pLiteralP :: Parser es (Pat (Malgo Parse))
pLiteralP = captureRange do
  boxed <- pBoxed
  sharp <- optional (symbol "#")
  pure $ \range -> case sharp of
    Just _ -> UnboxedP range $ coerce boxed
    Nothing -> BoxedP range boxed

-- * Lexical Analysis

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

-- * Combinators

-- | optional tries to parse the given parser and returns `Nothing` if it fails.
optional :: Parser es a -> Parser es (Maybe a)
optional p = try (fmap Just p) <|> pure Nothing

-- | manyUnaryOp parses zero or more unary operators and returns a function that applies them in order.
manyUnaryOp :: (MonadPlus f) => f (c -> c) -> f (c -> c)
manyUnaryOp singleUnaryOp = foldr1 (>>>) <$> some singleUnaryOp

-- | captureRange captures the source range of the parsed text.
captureRange :: (MonadParsec e s m, TraversableStream s) => m (Range -> b) -> m b
captureRange action = do
  start <- getSourcePos
  result <- action
  end <- getSourcePos
  pure $ result (Range start end)
