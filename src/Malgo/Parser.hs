{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Parser (parseMalgo) where

import Control.Monad.Combinators.Expr
import Control.Monad.Trans (lift)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.Lazy qualified as TL
import Data.Void
import Effectful
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Local (State, modify)
import Malgo.Module (ModuleName (..), Pragma, Workspace, insertPragmas, parseArtifactPath, pwdPath)
import Malgo.Prelude hiding (All)
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser es = ParsecT Void TL.Text (Eff es)

-- | Parse a module from a file.
parseMalgo ::
  (IOE :> es, Workspace :> es, State Pragma :> es) =>
  String ->
  TL.Text ->
  Eff
    es
    (Either (ParseErrorBundle TL.Text Void) (Module (Malgo Parse)))
parseMalgo srcPath text = runFileSystem do
  runParserT parser srcPath text
  where
    pragmas = extractPragmas text
    parser = do
      sc
      mod <- pModule
      eof
      lift $ modify $ insertPragmas mod.moduleName pragmas
      pure mod

-- | Extract pragmas from a module.
-- Returns the list of pragmas.
extractPragmas :: TL.Text -> [Text]
extractPragmas = go [] . TL.lines
  where
    go pragmas [] = map convertString $ reverse pragmas
    go pragmas (l : ls)
      | "#" `TL.isPrefixOf` l = go (l : pragmas) ls
      | otherwise = go pragmas ls

-- entry point
pModule :: (Workspace :> es, IOE :> es) => Parser es (Module (Malgo 'Parse))
pModule = do
  sourcePath <- (.sourceName) <$> getSourcePos
  pwd <- lift pwdPath
  sourcePath' <- lift $ parseArtifactPath pwd sourcePath
  ds <- many pDecl
  pure
    Module
      { moduleName = Artifact sourcePath',
        moduleDefinition = ParsedDefinitions ds
      }

-- module name
pModuleName :: (Workspace :> es, IOE :> es) => Parser es ModuleName
pModuleName = label "module path" $ asIdent <|> asPath
  where
    asIdent = ModuleName <$> lexeme (convertString <$> some identLetter)
    asPath = do
      path <- lexeme pString
      sourcePath <- (.sourceName) <$> getSourcePos
      pwd <- lift pwdPath
      sourcePath' <- lift $ parseArtifactPath pwd sourcePath
      path' <- lift $ parseArtifactPath sourcePath' path
      pure $ Artifact path'

-- toplevel declaration
pDecl :: (Workspace :> es, IOE :> es) => Parser es (Decl (Malgo 'Parse))
pDecl = do
  -- skip pragma
  _ <- optional $ pPragma
  pDataDef
    <|> pTypeSynonym
    <|> pInfix
    <|> pForeign
    <|> pImport
    <|> try (pScSig) -- try before 'pScDef'
    <|> pScDef

-- | pPragma just skips the pragma.
pPragma :: Parser es ()
pPragma = lexeme $ do
  void $ char '#'
  void $ takeWhileP (Just "pragma") (/= '\n')

pDataDef :: Parser es (Decl (Malgo 'Parse))
pDataDef = label "toplevel type definition" do
  void $ pKeyword "data"
  start <- getSourcePos
  d <- upperIdent
  xs <- many do
    start <- getSourcePos
    x <- lowerIdent
    end <- getSourcePos
    pure (Range start end, x)
  void $ pOperator "="
  ts <- pConDef `sepBy` pOperator "|"
  end <- getSourcePos
  pure $ DataDef (Range start end) d xs ts
  where
    pConDef = do
      start <- getSourcePos
      x <- upperIdent
      params <- many pSingleType
      end <- getSourcePos
      pure (Range start end, x, params)

pTypeSynonym :: Parser es (Decl (Malgo 'Parse))
pTypeSynonym = label "toplevel type synonym" do
  void $ pKeyword "type"
  start <- getSourcePos
  d <- upperIdent
  xs <- many lowerIdent
  void $ pOperator "="
  t <- pType
  end <- getSourcePos
  pure $ TypeSynonym (Range start end) d xs t

pInfix :: Parser es (Decl (Malgo 'Parse))
pInfix = label "infix declaration" do
  start <- getSourcePos
  a <-
    try (pKeyword "infixl" $> LeftA)
      <|> try (pKeyword "infixr" $> RightA)
      <|> pKeyword "infix"
      $> NeutralA
  i <- lexeme L.decimal
  x <- between (symbol "(") (symbol ")") operator
  end <- getSourcePos
  pure $ Infix (Range start end) a i x

pForeign :: Parser es (Decl (Malgo 'Parse))
pForeign = label "foreign import" do
  start <- getSourcePos
  void $ pKeyword "foreign"
  void $ pKeyword "import"
  x <- lowerIdent
  void $ pOperator ":"
  t <- pType
  end <- getSourcePos
  pure $ Foreign (Range start end) x t

pImport :: (Workspace :> es, IOE :> es) => Parser es (Decl (Malgo 'Parse))
pImport = label "import" do
  start <- getSourcePos
  void $ pKeyword "module"
  importList <-
    try (between (symbol "{") (symbol "}") importAll)
      <|> between (symbol "{") (symbol "}") importSelected
      <|> As
      <$> pModuleName
  void $ pOperator "="
  void $ pKeyword "import"
  modName <- pModuleName
  end <- getSourcePos
  pure $ Import (Range start end) modName importList
  where
    importAll = symbol ".." >> pure All
    importSelected = Selected <$> (lowerIdent <|> upperIdent <|> between (symbol "(") (symbol ")") operator) `sepBy` symbol ","

pScSig :: Parser es (Decl (Malgo 'Parse))
pScSig =
  label "toplevel function signature" do
    start <- getSourcePos
    void $ pKeyword "def"
    name <- lowerIdent <|> between (symbol "(") (symbol ")") operator
    void $ pOperator ":"
    typ <- pType
    end <- getSourcePos
    pure $ ScSig (Range start end) name typ

pScDef :: (Workspace :> es, IOE :> es) => Parser es (Decl (Malgo 'Parse))
pScDef =
  label "toplevel function definition" do
    start <- getSourcePos
    void $ pKeyword "def"
    name <- lowerIdent <|> between (symbol "(") (symbol ")") operator
    void $ pOperator "="
    exp <- pExpr
    end <- getSourcePos
    pure $ ScDef (Range start end) name exp

-- Expressions

pExpr :: (Workspace :> es, IOE :> es) => Parser es (Expr (Malgo 'Parse))
pExpr = do
  start <- getSourcePos
  expr <- pOpApp
  -- try type annotation for expression before just simple expression
  try (pAnnotation start expr)
    <|> pure expr
  where
    pAnnotation start expr = do
      _ <- pOperator ":"
      ty <- pType
      end <- getSourcePos
      pure $ Ann (Range start end) expr ty

pBoxed :: Parser es (Literal Boxed)
pBoxed =
  label "boxed literal"
    $ lexeme
    $ asum
      [ try (Float <$> (L.float <* string' "F")),
        try (Double <$> L.float),
        try (Int64 <$> (L.decimal <* string' "L")),
        Int32 <$> L.decimal,
        Char <$> between (char '\'') (char '\'') L.charLiteral,
        String . convertString <$> pString
      ]

pUnboxed :: Parser es (Literal Unboxed)
pUnboxed =
  label
    "unboxed literal"
    $ lexeme
    $ asum
      [ try (Double <$> (L.float <* char '#')),
        try (Float <$> (L.float <* string' "F#")),
        try (Int32 <$> (L.decimal <* char '#')),
        Int64 <$> (L.decimal <* string' "L#"),
        Char <$> (between (char '\'') (char '\'') L.charLiteral <* char '#'),
        String . convertString <$> (pString <* char '#')
      ]

pString :: (Token s ~ Char, MonadParsec e s m) => m [Char]
pString = label "string literal" do
  void $ char '"'
  manyTill L.charLiteral (char '"')

pVariable :: (Workspace :> es, IOE :> es) => Parser es (Expr (Malgo 'Parse))
pVariable =
  -- try full path identifier like `Foo.bar`
  -- before simple identifier like `bar`
  try pExplicitVariable <|> pImplicitVariable
  where
    pExplicitVariable = label "explicit variable" $ do
      start <- getSourcePos
      qualifier <- Explicit <$> pModuleName
      _ <- char '.'
      name <- lowerIdent <|> upperIdent
      end <- getSourcePos
      pure $ Var (Qualified qualifier (Range start end)) name

    pImplicitVariable = label "implicit variable" $ do
      start <- getSourcePos
      name <- lowerIdent <|> upperIdent
      end <- getSourcePos
      pure $ Var (Qualified Implicit (Range start end)) name

pFun :: (Workspace :> es, IOE :> es) => Parser es (Expr (Malgo 'Parse))
pFun =
  label "function literal" $ between (symbol "{") (symbol "}") do
    start <- getSourcePos
    clauses <- NonEmpty.fromList <$> pClauses
    end <- getSourcePos
    pure $ Fn (Range start end) clauses

-- pat1 -> exp1 , pat2 -> exp 2 , ...
-- last `,` is optional
pClauses :: (Workspace :> es, IOE :> es) => Parser es [Clause (Malgo 'Parse)]
pClauses = do
  pClause `sepEndBy1` pOperator ","

-- a clause is 'pat -> exp' or 'exp'
pClause :: (Workspace :> es, IOE :> es) => Parser es (Clause (Malgo 'Parse))
pClause = do
  start <- getSourcePos
  pat <- try (some pSinglePat <* pOperator "->") <|> pure []
  stmts <- do
    start <- getSourcePos
    stmts <- pStmts
    end <- getSourcePos
    pure $ Seq (Range start end) stmts
  end <- getSourcePos
  pure $ Clause (Range start end) pat stmts

pStmts :: (Workspace :> es, IOE :> es) => Parser es (NonEmpty (Stmt (Malgo 'Parse)))
pStmts = NonEmpty.fromList <$> pStmt `sepBy1` pOperator ";"

pStmt :: (Workspace :> es, IOE :> es) => Parser es (Stmt (Malgo 'Parse))
pStmt = try pLet <|> pWith <|> pNoBind

pLet :: (Workspace :> es, IOE :> es) => Parser es (Stmt (Malgo 'Parse))
pLet = do
  start <- getSourcePos
  void $ pKeyword "let"
  v <- lowerIdent
  void $ pOperator "="
  exp <- pExpr
  end <- getSourcePos
  pure $ Let (Range start end) v exp

pWith :: (Workspace :> es, IOE :> es) => Parser es (Stmt (Malgo 'Parse))
pWith = label "with" do
  start <- getSourcePos
  void $ pKeyword "with"
  asum
    [ try do
        var <- lowerIdent
        void $ pOperator "="
        exp <- pExpr
        end <- getSourcePos
        pure $ With (Range start end) (Just var) exp,
      do
        e <- pExpr
        end <- getSourcePos
        pure $ With (Range start end) Nothing e
    ]

pNoBind :: (Workspace :> es, IOE :> es) => Parser es (Stmt (Malgo 'Parse))
pNoBind = do
  start <- getSourcePos
  e <- pExpr
  end <- getSourcePos
  pure $ NoBind (Range start end) e

pRecordP :: Parser es (Pat (Malgo 'Parse))
pRecordP = do
  start <- getSourcePos
  kvs <- between (symbol "{") (symbol "}") do
    asRecordFields pRecordPEntry
  end <- getSourcePos
  pure $ RecordP (Range start end) kvs
  where
    pRecordPEntry = do
      label <- lowerIdent
      void $ pOperator "="
      value <- pPat
      pure (label, value)

pSinglePat :: Parser es (Pat (Malgo 'Parse))
pSinglePat =
  pVarP
    <|> pConP
    <|> try pUnboxedP
    <|> pBoxedP
    <|> try pTupleP
    <|> try pUnitP
    <|> pRecordP
    <|> pListP
    <|> between (symbol "(") (symbol ")") pPat
  where
    pVarP = do
      start <- getSourcePos
      name <- lowerIdent
      end <- getSourcePos
      pure $ VarP (Range start end) name
    pConP = do
      start <- getSourcePos
      name <- upperIdent
      end <- getSourcePos
      pure $ ConP (Range start end) name []
    pUnboxedP = do
      start <- getSourcePos
      u <- pUnboxed
      end <- getSourcePos
      pure $ UnboxedP (Range start end) u
    pBoxedP = do
      start <- getSourcePos
      b <- pBoxed
      end <- getSourcePos
      pure $ BoxedP (Range start end) b
    pTupleP = do
      start <- getSourcePos
      ps <- between (symbol "(") (symbol ")") do
        p <- pPat
        void $ pOperator ","
        (p :) <$> pPat `sepBy1` pOperator ","
      end <- getSourcePos
      pure $ TupleP (Range start end) ps
    pUnitP = do
      start <- getSourcePos
      _ <- symbol "("
      _ <- symbol ")"
      end <- getSourcePos
      pure $ TupleP (Range start end) []
    pListP = do
      start <- getSourcePos
      ps <- between (symbol "[") (symbol "]") $ pPat `sepBy` pOperator ","
      end <- getSourcePos
      pure $ ListP (Range start end) ps

pPat :: Parser es (Pat (Malgo 'Parse))
pPat =
  label "pattern" $ try pConP <|> pSinglePat
  where
    pConP = do
      start <- getSourcePos
      name <- upperIdent
      ps <- some pSinglePat
      end <- getSourcePos
      pure $ ConP (Range start end) name ps

pTuple :: (Workspace :> es, IOE :> es) => Parser es (Expr (Malgo 'Parse))
pTuple = label "tuple" do
  start <- getSourcePos
  xs <- between (symbol "(") (symbol ")") do
    x <- pExpr
    void $ pOperator ","
    xs <- pExpr `sepBy` pOperator ","
    pure (x : xs)
  end <- getSourcePos
  pure $ Tuple (Range start end) xs

pUnit :: Parser es (Expr (Malgo 'Parse))
pUnit = do
  start <- getSourcePos
  _ <- symbol "("
  _ <- symbol ")"
  end <- getSourcePos
  pure $ Tuple (Range start end) []

pRecord :: (Workspace :> es, IOE :> es) => Parser es (Expr (Malgo 'Parse))
pRecord = do
  start <- getSourcePos
  kvs <- between (symbol "{") (symbol "}") do
    asRecordFields pRecordEntry
  end <- getSourcePos
  pure $ Record (Range start end) kvs
  where
    pRecordEntry = do
      label <- lowerIdent
      void $ pOperator "="
      value <- pExpr
      pure (label, value)

pList :: (Workspace :> es, IOE :> es) => Parser es (Expr (Malgo 'Parse))
pList = label "list" do
  start <- getSourcePos
  xs <- between (symbol "[") (symbol "]") do
    pExpr `sepBy` pOperator ","
  end <- getSourcePos
  pure $ List (Range start end) xs

pSeq :: (Workspace :> es, IOE :> es) => Parser es (Expr (Malgo 'Parse))
pSeq = do
  start <- getSourcePos
  stmts <- between (symbol "(") (symbol ")") pStmts
  end <- getSourcePos
  pure $ Seq (Range start end) stmts

pSingleExpr :: (Workspace :> es, IOE :> es) => Parser es (Expr (Malgo 'Parse))
pSingleExpr =
  try pUnboxedExpr
    <|> try pBoxedExpr
    <|> pVariable
    <|> try pUnit
    <|> try pTuple
    <|> try pRecord
    <|> try pList
    <|> pList
    <|> pFun
    <|> try pSeq
    <|> pParens
  where
    pUnboxedExpr = do
      start <- getSourcePos
      u <- pUnboxed
      end <- getSourcePos
      pure $ Unboxed (Range start end) u
    pBoxedExpr = do
      start <- getSourcePos
      b <- pBoxed
      end <- getSourcePos
      pure $ Boxed (Range start end) b
    pParens = do
      start <- getSourcePos
      e <- between (symbol "(") (symbol ")") pExpr
      end <- getSourcePos
      pure $ Parens (Range start end) e

pApply :: (Workspace :> es, IOE :> es) => Parser es (Expr (Malgo 'Parse))
pApply = do
  start <- getSourcePos
  f <- pSingleExpr
  xs <- some pSingleExpr
  end <- getSourcePos
  pure $ foldl (Apply (Range start end)) f xs

pTerm :: (Workspace :> es, IOE :> es) => Parser es (Expr (Malgo 'Parse))
pTerm = try pApply <|> pSingleExpr

pOpApp :: (Workspace :> es, IOE :> es) => Parser es (Expr (Malgo 'Parse))
pOpApp = makeExprParser pTerm opTable
  where
    opTable =
      [ [ InfixL do
            start <- getSourcePos
            op <- operator
            end <- getSourcePos
            pure $ \l r -> OpApp (Range start end) op l r
        ]
      ]

-- Types

pType :: Parser es (Type (Malgo 'Parse))
pType = try pTyArr <|> pTyTerm

pTyVar :: Parser es (Type (Malgo 'Parse))
pTyVar = label "type variable" do
  start <- getSourcePos
  name <- lowerIdent
  end <- getSourcePos
  pure $ TyVar (Range start end) name

pTyCon :: Parser es (Type (Malgo 'Parse))
pTyCon = label "type constructor" do
  start <- getSourcePos
  name <- upperIdent
  end <- getSourcePos
  pure $ TyCon (Range start end) name

pTyTuple :: Parser es (Type (Malgo 'Parse))
pTyTuple = do
  start <- getSourcePos
  xs <- between (symbol "(") (symbol ")") do
    x <- pType
    void $ pOperator ","
    xs <- pType `sepBy` pOperator ","
    pure (x : xs)
  end <- getSourcePos
  pure $ TyTuple (Range start end) xs

pTyUnit :: Parser es (Type (Malgo 'Parse))
pTyUnit = do
  start <- getSourcePos
  _ <- symbol "("
  _ <- symbol ")"
  end <- getSourcePos
  pure $ TyTuple (Range start end) []

pTyRecord :: Parser es (Type (Malgo 'Parse))
pTyRecord = do
  start <- getSourcePos
  kvs <- between (symbol "{") (symbol "}") do
    asRecordFields pTyRecordEntry
  end <- getSourcePos
  pure $ TyRecord (Range start end) kvs
  where
    pTyRecordEntry = do
      label <- lowerIdent
      void $ pOperator ":"
      value <- pType
      pure (label, value)

pTyBlock :: Parser es (Type (Malgo 'Parse))
pTyBlock = do
  start <- getSourcePos
  _ <- symbol "{"
  t <- pType
  _ <- symbol "}"
  end <- getSourcePos
  pure $ TyBlock (Range start end) t

pSingleType :: Parser es (Type (Malgo 'Parse))
pSingleType =
  pTyVar
    <|> pTyCon
    <|> try pTyUnit
    <|> try pTyTuple
    <|> try pTyRecord
    <|> pTyBlock
    <|> between (symbol "(") (symbol ")") pType

pTyApp :: Parser es (Type (Malgo 'Parse))
pTyApp = do
  start <- getSourcePos
  f <- pSingleType
  xs <- some pSingleType
  end <- getSourcePos
  pure $ TyApp (Range start end) f xs

pTyTerm :: Parser es (Type (Malgo 'Parse))
pTyTerm = try pTyApp <|> pSingleType

pTyArr :: Parser es (Type (Malgo 'Parse))
pTyArr = makeExprParser pTyTerm opTable
  where
    opTable =
      [ [ InfixR do
            start <- getSourcePos
            void $ pOperator "->"
            end <- getSourcePos
            pure $ \l r -> TyArr (Range start end) l r
        ]
      ]

-- combinators

sc :: Parser es ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")
{-# INLINE sc #-}

lexeme :: Parser es a -> Parser es a
lexeme = L.lexeme sc

symbol :: TL.Text -> Parser es ()
symbol = void . L.symbol sc
{-# INLINE symbol #-}

identLetter :: Parser es Char
identLetter = alphaNumChar <|> oneOf ("_#" :: String)

opLetter :: Parser es Char
opLetter = oneOf ("+-*/\\%=><:;|&!#." :: String)

pKeyword :: TL.Text -> Parser es ()
pKeyword keyword = void $ lexeme (string keyword <* notFollowedBy identLetter)

pOperator :: TL.Text -> Parser es ()
pOperator op = void $ lexeme (string op <* notFollowedBy opLetter)

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

reserved :: Parser es ()
reserved = choice $ map (try . pKeyword) reservedKeywords -- #| and |# are for block comments in Core

reservedOperators :: [TL.Text]
reservedOperators = ["=>", "=", ":", "|", "->", ";", ",", "!", "#|", "|#"]

reservedOp :: Parser es ()
reservedOp = choice $ map (try . pOperator) reservedOperators

lowerIdent :: Parser es Text
lowerIdent =
  label "lower identifier"
    $ lexeme do
      notFollowedBy reserved
      convertString <$> ((:) <$> (lowerChar <|> char '_') <*> many identLetter)

upperIdent :: Parser es Text
upperIdent =
  label "upper identifier"
    $ lexeme do
      notFollowedBy reserved
      convertString <$> ((:) <$> upperChar <*> many identLetter)

operator :: Parser es Text
operator =
  label "operator"
    $ lexeme do
      notFollowedBy reservedOp
      convertString <$> some opLetter

-- { _ , _ , ... , _ } or { _ , _ , ... , _ , }
asRecordFields :: Parser es a -> Parser es [a]
asRecordFields entry = entry `sepEndBy1` pOperator ","
