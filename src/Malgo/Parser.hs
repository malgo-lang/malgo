module Malgo.Parser (parseMalgo) where

import Control.Monad.Combinators.Expr
import Data.Foldable (foldl)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Void
import Koriel.Id (ModuleName (ModuleName))
import Malgo.Prelude hiding
  ( many,
    some,
  )
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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
  ds <- between (symbol "{") (symbol "}") $ pDecl `endBy` pOperator ";"
  pure $ Module {_moduleName = ModuleName x, _moduleDefinition = ParsedDefinitions ds}

-- module name
pModuleName :: Parser Text
pModuleName = lexeme $ toText <$> some identLetter

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
  end <- getSourcePos
  xs <- many do
    start <- getSourcePos
    x <- lowerIdent
    end <- getSourcePos
    pure Annotated {_ann = Range start end, _value = x}
  void $ pOperator "="
  ts <- pConDef `sepBy` pOperator "|"
  pure $ DataDef (Range start end) d xs ts
  where
    pConDef = do
      start <- getSourcePos
      x <- upperIdent
      end <- getSourcePos
      (Range start end,x,) <$> many pSingleType

pTypeSynonym :: Parser (Decl (Malgo 'Parse))
pTypeSynonym = label "toplevel type synonym" do
  void $ pKeyword "type"
  start <- getSourcePos
  d <- upperIdent
  end <- getSourcePos
  xs <- many lowerIdent
  void $ pOperator "="
  TypeSynonym (Range start end) d xs <$> pType

pInfix :: Parser (Decl (Malgo 'Parse))
pInfix = label "infix declaration" do
  a <-
    try (pKeyword "infixl" $> LeftA)
      <|> try (pKeyword "infixr" $> RightA)
      <|> pKeyword "infix" $> NeutralA
  i <- lexeme L.decimal
  start <- getSourcePos
  x <- between (symbol "(") (symbol ")") operator
  end <- getSourcePos
  pure $ Infix (Range start end) a i x

pForeign :: Parser (Decl (Malgo 'Parse))
pForeign = label "foreign import" do
  void $ pKeyword "foreign"
  void $ pKeyword "import"
  start <- getSourcePos
  x <- lowerIdent
  end <- getSourcePos
  void $ pOperator ":"
  Foreign (Range start end) x <$> pType

pImport :: Parser (Decl (Malgo 'Parse))
pImport = label "import" do
  void $ pKeyword "module"
  importList <-
    try (between (symbol "{") (symbol "}") importAll)
      <|> between (symbol "{") (symbol "}") importSelected
      <|> As . ModuleName <$> pModuleName
  void $ pOperator "="
  void $ pKeyword "import"
  start <- getSourcePos
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
    name <- lowerIdent <|> between (symbol "(") (symbol ")") operator
    end <- getSourcePos
    void $ pOperator ":"
    ScSig (Range start end) name <$> pType

pScDef :: Parser (Decl (Malgo 'Parse))
pScDef =
  label "toplevel function definition" do
    start <- getSourcePos
    name <- lowerIdent <|> between (symbol "(") (symbol ")") operator
    end <- getSourcePos
    void $ pOperator "="
    ScDef (Range start end) name <$> pExp

-- Expressions

pExp :: Parser (Exp (Malgo 'Parse))
pExp = do
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
      <|> lexeme (String . toText <$> (char '"' *> manyTill L.charLiteral (char '"')))

pUnboxed :: Parser (Literal Unboxed)
pUnboxed =
  label "unboxed literal" $
    try (Double <$> lexeme (L.float <* char '#'))
      <|> try (Float <$> lexeme (L.float <* string' "F#"))
      <|> try (Int32 <$> lexeme (L.decimal <* char '#'))
      <|> Int64 <$> lexeme (L.decimal <* string' "L#")
      <|> lexeme (Char <$> (between (char '\'') (char '\'') L.charLiteral <* char '#'))
      <|> lexeme (String . toText <$> (char '"' *> manyTill L.charLiteral (char '"') <* char '#'))

pWithPrefix :: Parser Text -> Parser x -> Parser (Maybe Text, x)
pWithPrefix prefix body =
  -- try full path identifier like `Foo.bar`
  -- before simple identifier like `bar`
  try ((,) <$> (Just <$> prefix) <* char '.' <*> body)
    <|> (Nothing,) <$> body

pVariable :: Parser (Exp (Malgo 'Parse))
pVariable =
  label "variable" do
    start <- getSourcePos
    (prefix, name) <- pWithPrefix pModuleName (lowerIdent <|> upperIdent)
    end <- getSourcePos
    pure $ Var (WithPrefix $ Annotated prefix (Range start end)) name

pFun :: Parser (Exp (Malgo 'Parse))
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
  void $ pKeyword "let"
  start <- getSourcePos
  v <- lowerIdent
  end <- getSourcePos
  void $ pOperator "="
  Let (Range start end) v <$> pExp

pWith :: Parser (Stmt (Malgo 'Parse))
pWith = label "with" do
  void $ pKeyword "with"
  start <- getSourcePos
  asum
    [ try do
        var <- lowerIdent
        void $ pOperator "="
        end <- getSourcePos
        With (Range start end) (Just var) <$> pExp,
      do
        e <- pExp
        end <- getSourcePos
        pure $ With (Range start end) Nothing e
    ]

pNoBind :: Parser (Stmt (Malgo 'Parse))
pNoBind = do
  start <- getSourcePos
  e <- pExp
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
      (prefix, label) <- pWithPrefix upperIdent lowerIdent
      void $ pOperator "="
      value <- pPat
      pure (WithPrefix $ Annotated prefix label, value)

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

pTuple :: Parser (Exp (Malgo 'Parse))
pTuple = label "tuple" do
  start <- getSourcePos
  xs <- between (symbol "(") (symbol ")") do
    x <- pExp
    void $ pOperator ","
    xs <- pExp `sepBy` pOperator ","
    pure (x : xs)
  end <- getSourcePos
  pure $ Tuple (Range start end) xs

pUnit :: Parser (Exp (Malgo 'Parse))
pUnit = do
  start <- getSourcePos
  _ <- symbol "("
  _ <- symbol ")"
  end <- getSourcePos
  pure $ Tuple (Range start end) []

pRecord :: Parser (Exp (Malgo 'Parse))
pRecord = do
  start <- getSourcePos
  kvs <- between (symbol "{") (symbol "}") do
    asRecordFields pRecordEntry
  end <- getSourcePos
  pure $ Record (Range start end) kvs
  where
    pRecordEntry = do
      (prefix, label) <- pWithPrefix upperIdent lowerIdent
      void $ pOperator "="
      value <- pExp
      pure (WithPrefix $ Annotated prefix label, value)

pList :: Parser (Exp (Malgo 'Parse))
pList = label "list" do
  start <- getSourcePos
  xs <- between (symbol "[") (symbol "]") do
    pExp `sepBy` pOperator ","
  end <- getSourcePos
  pure $ List (Range start end) xs

pRecordAccess :: Parser (Exp (Malgo 'Parse))
pRecordAccess = do
  start <- getSourcePos
  (prefix, l) <- char '#' >> pWithPrefix upperIdent lowerIdent
  end <- getSourcePos
  pure $ RecordAccess (Range start end) (WithPrefix (Annotated prefix l))


pSeq :: Parser (Exp (Malgo 'Parse))
pSeq = do
  start <- getSourcePos
  stmts <- between (symbol "(") (symbol ")") pStmts
  end <- getSourcePos
  pure $ Seq (Range start end) stmts

pSingleExp :: Parser (Exp (Malgo 'Parse))
pSingleExp =
  try pUnboxedExp
    <|> try pBoxedExp
    <|> pVariable
    <|> try pUnit
    <|> try pTuple
    <|> try pRecord
    <|> try pList
    <|> pList
    <|> pFun
    <|> pRecordAccess
    <|> try pSeq
    <|> pParens
  where
    pUnboxedExp = do
      start <- getSourcePos
      u <- pUnboxed
      end <- getSourcePos
      pure $ Unboxed (Range start end) u
    pBoxedExp = do
      start <- getSourcePos
      b <- pBoxed
      end <- getSourcePos
      pure $ Boxed (Range start end) b
    pParens = do
      start <- getSourcePos
      e <- between (symbol "(") (symbol ")") pExp
      end <- getSourcePos
      pure $ Parens (Range start end) e

pApply :: Parser (Exp (Malgo 'Parse))
pApply = do
  start <- getSourcePos
  f <- pSingleExp
  xs <- some pSingleExp
  end <- getSourcePos
  pure $ foldl (Apply (Range start end)) f xs

pTerm :: Parser (Exp (Malgo 'Parse))
pTerm = try pApply <|> pSingleExp

pOpApp :: Parser (Exp (Malgo 'Parse))
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
identLetter = alphaNumChar <|> oneOf ("_#'" :: String)

opLetter :: Parser Char
opLetter = oneOf ("+-*/\\%=><:;|&!#." :: String)

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy identLetter)

pOperator :: Text -> Parser Text
pOperator op = lexeme (string op <* notFollowedBy opLetter)

reserved :: Parser Text
reserved =
  choice $
    map
      (try . pKeyword)
      [ "class",
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

reservedOp :: Parser Text
reservedOp = choice $ map (try . pOperator) ["=>", "=", ":", "|", "->", ";", ",", "!"]

lowerIdent :: Parser Text
lowerIdent =
  label "lower identifier" $
    lexeme do
      notFollowedBy reserved <|> notReserved
      toText <$> ((:) <$> (lowerChar <|> char '_') <*> many identLetter)

upperIdent :: Parser Text
upperIdent =
  label "upper identifier" $
    lexeme do
      notFollowedBy reserved <|> notReserved
      toText <$> ((:) <$> upperChar <*> many identLetter)

operator :: Parser Text
operator =
  label "operator" $
    lexeme do
      notFollowedBy reservedOp
      toText <$> some opLetter

notReserved :: Parser ()
notReserved = do
  word <- lookAhead reserved
  registerFancyFailure (one $ ErrorFail $ "unexpected '" <> toString word <> "'\nThis is a reserved keyword")

-- { _ , _ , ... , _ } or { _ ; _ ; ... ; _ ; }
-- `;` terminatorによる分割を先に検討して、その後`,` separatorによる分割を検討する
-- 逆だと１要素の`,` separatorを読んでしまい、パースが失敗する
asRecordFields :: Parser a -> Parser [a]
asRecordFields entry = try (entry `endBy1` pOperator ";") <|> (entry `sepBy1` pOperator ",")