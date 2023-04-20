{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Parser (parseMalgo) where

import Control.Monad.Combinators.Expr
import Data.List.NonEmpty qualified as NonEmpty
import Data.Void
import Koriel.Id (ModuleName (ModuleName))
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

-- | パーサー
--
-- ファイル1つにつきモジュール1つ
parseMalgo :: String -> Text -> Either (ParseErrorBundle Text Void) (Module (Malgo 'Parse))
parseMalgo = parse do
  sc
  mod <- pModule
  eof
  pure mod

-- entry point
pModule :: Parser (Module (Malgo 'Parse))
pModule = do
  void $ pKeyword "module"
  x <- pModuleName
  void $ pOperator "="
  ds <- between (symbol "{") (symbol "}") $ many pDecl
  pure $ Module {_moduleName = ModuleName x, _moduleDefinition = ParsedDefinitions ds}

-- module name
pModuleName :: Parser Text
pModuleName = lexeme $ convertString <$> some identLetter

-- toplevel declaration
pDecl :: Parser (Decl (Malgo 'Parse))
pDecl =
  pDataDef
    <|> pTypeSynonym
    <|> pInfix
    <|> pForeign
    <|> pImport
    <|> try pScSig -- try before 'pScDef'
    <|> pScDef

pDataDef :: Parser (Decl (Malgo 'Parse))
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

pTypeSynonym :: Parser (Decl (Malgo 'Parse))
pTypeSynonym = label "toplevel type synonym" do
  void $ pKeyword "type"
  start <- getSourcePos
  d <- upperIdent
  xs <- many lowerIdent
  void $ pOperator "="
  t <- pType
  end <- getSourcePos
  pure $ TypeSynonym (Range start end) d xs t

pInfix :: Parser (Decl (Malgo 'Parse))
pInfix = label "infix declaration" do
  start <- getSourcePos
  a <-
    try (pKeyword "infixl" $> LeftA)
      <|> try (pKeyword "infixr" $> RightA)
      <|> pKeyword "infix" $> NeutralA
  i <- lexeme L.decimal
  x <- between (symbol "(") (symbol ")") operator
  end <- getSourcePos
  pure $ Infix (Range start end) a i x

pForeign :: Parser (Decl (Malgo 'Parse))
pForeign = label "foreign import" do
  start <- getSourcePos
  void $ pKeyword "foreign"
  void $ pKeyword "import"
  x <- lowerIdent
  void $ pOperator ":"
  t <- pType
  end <- getSourcePos
  pure $ Foreign (Range start end) x t

pImport :: Parser (Decl (Malgo 'Parse))
pImport = label "import" do
  start <- getSourcePos
  void $ pKeyword "module"
  importList <-
    try (between (symbol "{") (symbol "}") importAll)
      <|> between (symbol "{") (symbol "}") importSelected
      <|> As . ModuleName <$> pModuleName
  void $ pOperator "="
  void $ pKeyword "import"
  modName <- ModuleName <$> pModuleName
  end <- getSourcePos
  pure $ Import (Range start end) modName importList
  where
    importAll = symbol ".." >> pure All
    importSelected = Selected <$> (lowerIdent <|> upperIdent <|> between (symbol "(") (symbol ")") operator) `sepBy` symbol ","

pScSig :: Parser (Decl (Malgo 'Parse))
pScSig =
  label "toplevel function signature" do
    start <- getSourcePos
    void $ pKeyword "def"
    name <- lowerIdent <|> between (symbol "(") (symbol ")") operator
    void $ pOperator ":"
    typ <- pType
    end <- getSourcePos
    pure $ ScSig (Range start end) name typ

pScDef :: Parser (Decl (Malgo 'Parse))
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

pExpr :: Parser (Expr (Malgo 'Parse))
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

pBoxed :: Parser (Literal Boxed)
pBoxed =
  label "boxed literal" $
    try (Float <$> lexeme (L.float <* string' "F"))
      <|> try (Double <$> lexeme L.float)
      <|> try (Int64 <$> lexeme (L.decimal <* string' "L"))
      <|> Int32 <$> lexeme L.decimal
      <|> lexeme (Char <$> between (char '\'') (char '\'') L.charLiteral)
      <|> lexeme (String . convertString <$> (char '"' *> manyTill L.charLiteral (char '"')))

pUnboxed :: Parser (Literal Unboxed)
pUnboxed =
  label "unboxed literal" $
    try (Double <$> lexeme (L.float <* char '#'))
      <|> try (Float <$> lexeme (L.float <* string' "F#"))
      <|> try (Int32 <$> lexeme (L.decimal <* char '#'))
      <|> Int64 <$> lexeme (L.decimal <* string' "L#")
      <|> lexeme (Char <$> (between (char '\'') (char '\'') L.charLiteral <* char '#'))
      <|> lexeme (String . convertString <$> (char '"' *> manyTill L.charLiteral (char '"') <* char '#'))

pVariable :: Parser (Expr (Malgo 'Parse))
pVariable =
  -- try full path identifier like `Foo.bar`
  -- before simple identifier like `bar`
  try pExplicitVariable <|> pImplicitVariable
  where
    pExplicitVariable :: Parser (Expr (Malgo 'Parse))
    pExplicitVariable = label "explicit variable" $ do
      start <- getSourcePos
      qualifier <- Explicit . ModuleName <$> pModuleName
      _ <- char '.'
      name <- lowerIdent <|> upperIdent
      end <- getSourcePos
      pure $ Var (Qualified qualifier (Range start end)) name

    pImplicitVariable :: Parser (Expr (Malgo 'Parse))
    pImplicitVariable = label "implicit variable" $ do
      start <- getSourcePos
      name <- lowerIdent <|> upperIdent
      end <- getSourcePos
      pure $ Var (Qualified Implicit (Range start end)) name

pFun :: Parser (Expr (Malgo 'Parse))
pFun =
  label "function literal" do
    start <- getSourcePos
    clauses <-
      between (symbol "{") (symbol "}") $
        NonEmpty.fromList <$> pClauses
    end <- getSourcePos
    pure $ Fn (Range start end) clauses

-- [|] pat1 -> exp1 | pat2 -> exp 2 | ...
-- first `|` is optional
pClauses :: Parser [Clause (Malgo 'Parse)]
pClauses = do
  _ <- optional (pOperator "|")
  pClause `sepBy1` pOperator "|"

-- a clause is 'pat -> exp' or 'exp'
pClause :: Parser (Clause (Malgo 'Parse))
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

pStmts :: Parser (NonEmpty (Stmt (Malgo 'Parse)))
pStmts = NonEmpty.fromList <$> pStmt `sepBy1` pOperator ";"

pStmt :: Parser (Stmt (Malgo 'Parse))
pStmt = try pLet <|> pWith <|> pNoBind

pLet :: Parser (Stmt (Malgo 'Parse))
pLet = do
  start <- getSourcePos
  void $ pKeyword "let"
  v <- lowerIdent
  void $ pOperator "="
  exp <- pExpr
  end <- getSourcePos
  pure $ Let (Range start end) v exp

pWith :: Parser (Stmt (Malgo 'Parse))
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

pNoBind :: Parser (Stmt (Malgo 'Parse))
pNoBind = do
  start <- getSourcePos
  e <- pExpr
  end <- getSourcePos
  pure $ NoBind (Range start end) e

pRecordP :: Parser (Pat (Malgo 'Parse))
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

pSinglePat :: Parser (Pat (Malgo 'Parse))
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

pPat :: Parser (Pat (Malgo 'Parse))
pPat =
  label "pattern" $ try pConP <|> pSinglePat
  where
    pConP = do
      start <- getSourcePos
      name <- upperIdent
      ps <- some pSinglePat
      end <- getSourcePos
      pure $ ConP (Range start end) name ps

pTuple :: Parser (Expr (Malgo 'Parse))
pTuple = label "tuple" do
  start <- getSourcePos
  xs <- between (symbol "(") (symbol ")") do
    x <- pExpr
    void $ pOperator ","
    xs <- pExpr `sepBy` pOperator ","
    pure (x : xs)
  end <- getSourcePos
  pure $ Tuple (Range start end) xs

pUnit :: Parser (Expr (Malgo 'Parse))
pUnit = do
  start <- getSourcePos
  _ <- symbol "("
  _ <- symbol ")"
  end <- getSourcePos
  pure $ Tuple (Range start end) []

pRecord :: Parser (Expr (Malgo 'Parse))
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

pList :: Parser (Expr (Malgo 'Parse))
pList = label "list" do
  start <- getSourcePos
  xs <- between (symbol "[") (symbol "]") do
    pExpr `sepBy` pOperator ","
  end <- getSourcePos
  pure $ List (Range start end) xs

pSeq :: Parser (Expr (Malgo 'Parse))
pSeq = do
  start <- getSourcePos
  stmts <- between (symbol "(") (symbol ")") pStmts
  end <- getSourcePos
  pure $ Seq (Range start end) stmts

pSingleExpr :: Parser (Expr (Malgo 'Parse))
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

pApply :: Parser (Expr (Malgo 'Parse))
pApply = do
  start <- getSourcePos
  f <- pSingleExpr
  xs <- some pSingleExpr
  end <- getSourcePos
  pure $ foldl (Apply (Range start end)) f xs

pTerm :: Parser (Expr (Malgo 'Parse))
pTerm = try pApply <|> pSingleExpr

pOpApp :: Parser (Expr (Malgo 'Parse))
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

pType :: Parser (Type (Malgo 'Parse))
pType = try pTyArr <|> pTyTerm

pTyVar :: Parser (Type (Malgo 'Parse))
pTyVar = label "type variable" do
  start <- getSourcePos
  name <- lowerIdent
  end <- getSourcePos
  pure $ TyVar (Range start end) name

pTyCon :: Parser (Type (Malgo 'Parse))
pTyCon = label "type constructor" do
  start <- getSourcePos
  name <- upperIdent
  end <- getSourcePos
  pure $ TyCon (Range start end) name

pTyTuple :: Parser (Type (Malgo 'Parse))
pTyTuple = do
  start <- getSourcePos
  xs <- between (symbol "(") (symbol ")") do
    x <- pType
    void $ pOperator ","
    xs <- pType `sepBy` pOperator ","
    pure (x : xs)
  end <- getSourcePos
  pure $ TyTuple (Range start end) xs

pTyUnit :: Parser (Type (Malgo 'Parse))
pTyUnit = do
  start <- getSourcePos
  _ <- symbol "("
  _ <- symbol ")"
  end <- getSourcePos
  pure $ TyTuple (Range start end) []

pTyRecord :: Parser (Type (Malgo 'Parse))
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

pTyBlock :: Parser (Type (Malgo 'Parse))
pTyBlock = do
  start <- getSourcePos
  _ <- symbol "{"
  t <- pType
  _ <- symbol "}"
  end <- getSourcePos
  pure $ TyBlock (Range start end) t

pSingleType :: Parser (Type (Malgo 'Parse))
pSingleType =
  pTyVar
    <|> pTyCon
    <|> try pTyUnit
    <|> try pTyTuple
    <|> try pTyRecord
    <|> pTyBlock
    <|> between (symbol "(") (symbol ")") pType

pTyApp :: Parser (Type (Malgo 'Parse))
pTyApp = do
  start <- getSourcePos
  f <- pSingleType
  xs <- some pSingleType
  end <- getSourcePos
  pure $ TyApp (Range start end) f xs

pTyTerm :: Parser (Type (Malgo 'Parse))
pTyTerm = try pTyApp <|> pSingleType

pTyArr :: Parser (Type (Malgo 'Parse))
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

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

identLetter :: Parser Char
identLetter = alphaNumChar <|> oneOf ("_#" :: String)

opLetter :: Parser Char
opLetter = oneOf ("+-*/\\%=><:;|&!#." :: String)

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy identLetter)

pOperator :: Text -> Parser Text
pOperator op = lexeme (string op <* notFollowedBy opLetter)

reservedKeywords :: [Text]
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

reserved :: Parser Text
reserved = choice $ map (try . pKeyword) reservedKeywords -- #| and |# are for block comments in Koriel

reservedOperators :: [Text]
reservedOperators = ["=>", "=", ":", "|", "->", ";", ",", "!", "#|", "|#"]

reservedOp :: Parser Text
reservedOp = choice $ map (try . pOperator) reservedOperators

lowerIdent :: Parser Text
lowerIdent =
  label "lower identifier" $
    lexeme do
      notFollowedBy reserved
      convertString <$> ((:) <$> (lowerChar <|> char '_') <*> many identLetter)

upperIdent :: Parser Text
upperIdent =
  label "upper identifier" $
    lexeme do
      notFollowedBy reserved
      convertString <$> ((:) <$> upperChar <*> many identLetter)

operator :: Parser Text
operator =
  label "operator" $
    lexeme do
      notFollowedBy reservedOp
      convertString <$> some opLetter

-- { _ , _ , ... , _ } or { _ ; _ ; ... ; _ ; }
-- `;` terminatorによる分割を先に検討して、その後`,` separatorによる分割を検討する
-- 逆だと１要素の`,` separatorを読んでしまい、パースが失敗する
asRecordFields :: Parser a -> Parser [a]
asRecordFields entry = try (entry `endBy1` pOperator ";") <|> (entry `sepBy1` pOperator ",")