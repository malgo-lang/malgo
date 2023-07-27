module Malgo.Parser.LayoutParser (parse) where

import Control.Exception (assert)
import Control.Monad.Combinators.Expr
import Control.Monad.Reader
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Koriel.Id
import Malgo.Parser.Stream (LexStream)
import Malgo.Parser.Stream qualified as L
import Malgo.Prelude hiding (All)
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec hiding (parse)
import Text.Megaparsec qualified as M
import Debug.Trace (traceM, traceShowM)
import Koriel.Pretty

type Parser = ReaderT Int (Parsec Void LexStream)

parse :: String -> LexStream -> Either (ParseErrorBundle LexStream Void) (Module (Malgo Parse))
parse =
  M.parse
    ( runReaderT ?? 0 $ do
        m <- pModule
        eof
        pure m
    )

-- * Module

pModule :: Parser (Module (Malgo Parse))
pModule = do
  reserved L.Module
  name <- pModuleName
  reservedOp L.Equal
  decls <- pDecls
  pure Module {moduleName = name, moduleDefinition = ParsedDefinitions decls}

pModuleName :: Parser ModuleName
pModuleName = ModuleName <$> ident

-- * Declaration

pDecls :: Parser [Decl (Malgo Parse)]
pDecls = between (reservedOp L.LBrace) (reservedOp L.RBrace) $ blocks pDecl

pDecl :: Parser (Decl (Malgo Parse))
pDecl =
  choice
    [ try pScDef,
      pScSig,
      pDataDef,
      pTypeSynonym,
      pInfix,
      pForeign,
      pImport
    ]

pScDef :: Parser (Decl (Malgo Parse))
pScDef = do
  start <- getSourcePos
  reserved L.Def
  name <- lowerIdent <|> between (reservedOp L.LParen) (reservedOp L.RParen) operator
  reservedOp L.Equal
  body <- pExpr
  end <- getSourcePos
  pure $ ScDef (Range start end) name body

pScSig :: Parser (Decl (Malgo Parse))
pScSig = do
  start <- getSourcePos
  reserved L.Def
  name <- lowerIdent <|> between (reservedOp L.LParen) (reservedOp L.RParen) operator
  reservedOp L.Colon
  ty <- pType
  end <- getSourcePos
  pure $ ScSig (Range start end) name ty

pDataDef :: Parser (Decl (Malgo Parse))
pDataDef = label "data" do
  start <- getSourcePos
  reserved L.Data
  name <- upperIdent
  args <- many do
    start <- getSourcePos
    arg <- lowerIdent
    end <- getSourcePos
    pure (Range start end, arg)
  reservedOp L.Equal
  cons <- blocks pConDef <|> pConDef `sepBy1` reservedOp L.Bar
  end <- getSourcePos
  pure $ DataDef (Range start end) name args cons
  where
    pConDef = do
      start <- getSourcePos
      name <- upperIdent
      params <- many pSingleType
      end <- getSourcePos
      pure (Range start end, name, params)

pTypeSynonym :: Parser (Decl (Malgo Parse))
pTypeSynonym = label "type" do
  start <- getSourcePos
  reserved L.Type
  name <- upperIdent
  args <- many lowerIdent
  reservedOp L.Equal
  ty <- pType
  end <- getSourcePos
  pure $ TypeSynonym (Range start end) name args ty

pInfix :: Parser (Decl (Malgo Parse))
pInfix = label "infix" do
  start <- getSourcePos
  assoc <- choice [reserved L.Infixl $> LeftA, reserved L.Infixr $> RightA, reserved L.Infix $> NeutralA]
  i <- int
  name <- between (reservedOp L.LParen) (reservedOp L.RParen) operator
  end <- getSourcePos
  pure $ Infix (Range start end) assoc i name

pForeign :: Parser (Decl (Malgo Parse))
pForeign = label "foreign import" do
  start <- getSourcePos
  reserved L.Foreign
  reserved L.Import
  x <- lowerIdent
  reservedOp L.Colon
  ty <- pType
  end <- getSourcePos
  pure $ Foreign (Range start end) x ty

pImport :: Parser (Decl (Malgo Parse))
pImport = label "import" do
  start <- getSourcePos
  reserved L.Module
  importList <- pImportList
  reservedOp L.Equal
  reserved L.Import
  name <- pModuleName
  end <- getSourcePos
  pure $ Import (Range start end) name importList
  where
    pImportList = importPartial <|> importPrefix
    importPartial = do
      between (reservedOp L.LBrace) (reservedOp L.RBrace)
        $ importAll
        <|> importSelected
    importAll = do
      reservedOp L.DotDot
      pure All
    importSelected = do
      Selected <$> choice [lowerIdent, upperIdent, between (reservedOp L.LParen) (reservedOp L.RParen) operator] `sepBy` reservedOp L.Comma
    importPrefix = As <$> pModuleName

-- * Expression

pExpr :: Parser (Expr (Malgo Parse))
pExpr = do
  start <- getSourcePos
  traceShowM $ "pExpr:" <+> pretty start
  expr <- pOpApp
  try (pAnnotation start expr)
    <|> pure expr
  where
    pOpApp = makeExprParser pTerm opTable
    pTerm = do
      start <- getSourcePos
      f <- pSingleExpr
      xs <- many pSingleExpr
      end <- getSourcePos
      case xs of
        [] -> pure f
        xs -> pure $ foldl (Apply (Range start end)) f xs
    opTable =
      [ [ InfixL do
            start <- getSourcePos
            op <- operator
            end <- getSourcePos
            pure $ OpApp (Range start end) op
        ]
      ]
    pSingleExpr =
      choice
        [ try pUnboxedExpr,
          try pBoxedExpr,
          pVar,
          pParen,
          pBrace,
          pList
        ]
    pUnboxedExpr = do
      start <- getSourcePos
      u <- pUnboxed
      end <- getSourcePos
      pure $ Unboxed (Range start end) u
    pBoxedExpr = do
      start <- getSourcePos
      u <- pBoxed
      end <- getSourcePos
      pure $ Boxed (Range start end) u
    pVar = pQualified <|> pUnqualified
      where
        pQualified :: Parser (Expr (Malgo Parse))
        pQualified = do
          start <- getSourcePos
          (moduleName, name) <- qualifiedIdent
          end <- getSourcePos
          pure $ Var (Qualified (Explicit $ ModuleName moduleName) (Range start end)) name
        pUnqualified = do
          start <- getSourcePos
          name <- ident
          end <- getSourcePos
          pure $ Var (Qualified Implicit (Range start end)) name
    pParen = do
      start <- getSourcePos
      xs <- between (reservedOp L.LParen) (reservedOp L.RParen) (pExpr `sepBy` reservedOp L.Comma)
      end <- getSourcePos
      case xs of
        [x] -> pure x
        xs -> pure $ Tuple (Range start end) xs
    pBrace = try pFun <|> pRecord
      where
        pRecord = do
          start <- getSourcePos
          kvs <- between (reservedOp L.LBrace) (reservedOp L.RBrace) do
            asList do
              label <- lowerIdent
              reservedOp L.Equal
              value <- pExpr
              pure (label, value)
          end <- getSourcePos
          pure $ Record (Range start end) kvs
    pList = do
      start <- getSourcePos
      xs <- between (reservedOp L.LBracket) (reservedOp L.RBracket) $ asList pExpr
      end <- getSourcePos
      pure $ List (Range start end) xs
    pAnnotation start expr = do
      reservedOp L.Colon
      ty <- pType
      end <- getSourcePos
      pure $ Ann (Range start end) expr ty

pFun :: Parser (Expr (Malgo Parse))
pFun = do
  start <- getSourcePos
  clauses <-
    between (reservedOp L.LBrace) (reservedOp L.RBrace) $ NonEmpty.fromList <$> pClauses
  end <- getSourcePos
  pure $ Fn (Range start end) clauses
  where
    pClauses =
      do
        blocks pClause
        <|> (optional (reservedOp L.Bar) >> pClause `sepBy1` reservedOp L.Bar)
    pClause :: Parser (Clause (Malgo Parse))
    pClause = do
      start <- getSourcePos
      traceShowM $ "pClause: " <> pretty start
      pat <- try (some pSinglePat <* reservedOp L.Arrow) <|> pure []
      traceShowM $ "pClause: " <> pretty pat
      stmts <- do
        start <- getSourcePos
        stmts <- pStmts
        end <- getSourcePos
        pure $ Seq (Range start end) stmts
      end <- getSourcePos
      pure $ Clause (Range start end) pat stmts
    pStmts = do
      NonEmpty.fromList <$> asList pStmt
    pStmt :: Parser (Stmt (Malgo Parse))
    pStmt = do
      pos <- getSourcePos
      traceShowM $ "pStmt: " <> pretty pos
      choice [pLet, pWith, pNoBind]
    pLet = do
      start <- getSourcePos
      reserved L.Let
      x <- lowerIdent
      reservedOp L.Equal
      e <- pExpr
      end <- getSourcePos
      pure $ Let (Range start end) x e
    pWith = do
      start <- getSourcePos
      reserved L.With
      choice
        [ try do
            x <- lowerIdent
            reservedOp L.Equal
            e <- pExpr
            end <- getSourcePos
            pure $ With (Range start end) (Just x) e,
          do
            e <- pExpr
            end <- getSourcePos
            pure $ With (Range start end) Nothing e
        ]
    pNoBind = do
      start <- getSourcePos
      traceShowM $ "pNoBind: " <> pretty start
      e <- pExpr
      end <- getSourcePos
      pure $ NoBind (Range start end) e

-- * Pattern

pPat :: Parser (Pat (Malgo Parse))
pPat = do
  try pConP <|> pSinglePat
  where
    pConP = do
      start <- getSourcePos
      name <- upperIdent
      args <- some pSinglePat
      end <- getSourcePos
      pure $ ConP (Range start end) name args

pSinglePat :: Parser (Pat (Malgo Parse))
pSinglePat = do
  pos <- getSourcePos
  traceShowM $ "pSinglePat: " <> pretty pos 
  x <- choice [pVarP, pConP, try pUnboxedP, pBoxedP, pParenP, pRecordP, pListP]
  traceShowM $ "pSinglePat: " <> pretty x
  pure x
  where
    pVarP :: Parser (Pat (Malgo Parse))
    pVarP = do
      start <- getSourcePos
      name <- lowerIdent
      end <- getSourcePos
      pure $ VarP (Range start end) name
    pConP :: Parser (Pat (Malgo Parse))
    pConP = do
      start <- getSourcePos
      name <- upperIdent
      end <- getSourcePos
      pure $ ConP (Range start end) name []
    pUnboxedP :: Parser (Pat (Malgo Parse))
    pUnboxedP = do
      start <- getSourcePos
      u <- pUnboxed
      end <- getSourcePos
      pure $ UnboxedP (Range start end) u
    pBoxedP :: Parser (Pat (Malgo Parse))
    pBoxedP = do
      start <- getSourcePos
      u <- pBoxed
      end <- getSourcePos
      pure $ BoxedP (Range start end) u
    pParenP = do
      start <- getSourcePos
      xs <- between (reservedOp L.LParen) (reservedOp L.RParen) (pPat `sepBy` reservedOp L.Comma)
      end <- getSourcePos
      case xs of
        [x] -> pure x
        xs -> pure $ TupleP (Range start end) xs
    pRecordP :: Parser (Pat (Malgo Parse))
    pRecordP = do
      start <- getSourcePos
      traceShowM $ "pRecordP: " <> pretty start
      kvs <- between (reservedOp L.LBrace) (reservedOp L.RBrace) do
        asList pRecordPEntry
      end <- getSourcePos
      pure $ RecordP (Range start end) kvs
      where
        pRecordPEntry = do
          pos <- getSourcePos
          label <- lowerIdent
          traceShowM $ "pRecordPEntry: " <> pretty pos <+> pretty label
          reservedOp L.Equal
          pat <- pPat
          pure (label, pat)
    pListP = do
      start <- getSourcePos
      xs <- between (reservedOp L.LBracket) (reservedOp L.RBracket) $ asList pPat
      end <- getSourcePos
      pure $ ListP (Range start end) xs

-- * Type

pType :: Parser (Type (Malgo Parse))
pType = makeExprParser pTyTerm opTable
  where
    opTable =
      [ [ InfixR do
            start <- getSourcePos
            reservedOp L.Arrow
            TyArr . Range start <$> getSourcePos
        ]
      ]
    pTyTerm = do
      start <- getSourcePos
      f <- pSingleType
      xs <- many pSingleType
      end <- getSourcePos
      case xs of
        [] -> pure f
        _ -> pure $ TyApp (Range start end) f xs

pSingleType :: Parser (Type (Malgo Parse))
pSingleType =
  choice
    [ pTyVar,
      pTyCon,
      pTyParen,
      pTyBrace
    ]
  where
    pTyVar = do
      start <- getSourcePos
      x <- lowerIdent
      end <- getSourcePos
      pure $ TyVar (Range start end) x
    pTyCon = do
      start <- getSourcePos
      x <- upperIdent
      end <- getSourcePos
      pure $ TyCon (Range start end) x
    pTyParen = do
      start <- getSourcePos
      xs <- between (reservedOp L.LParen) (reservedOp L.RParen) (pType `sepBy` reservedOp L.Comma)
      end <- getSourcePos
      case xs of
        [x] -> pure x
        xs -> pure $ TyTuple (Range start end) xs
    pTyBrace = do
      start <- getSourcePos
      k <- between (reservedOp L.LBrace) (reservedOp L.RBrace) (try pTyRecord <|> pTyBlock)
      k start <$> getSourcePos
    pTyRecord = do
      xs <- asList pTyRecordEntry
      pure \start end -> TyRecord (Range start end) xs
    pTyRecordEntry = do
      label <- lowerIdent
      reservedOp L.Colon
      ty <- pType
      pure (label, ty)
    pTyBlock = do
      x <- pType
      pure \start end -> TyBlock (Range start end) x

-- * common combinators

ident :: Parser Text
ident =
  lexeme
    $ token
    ?? mempty
    $ \case
      L.WithPos {value = L.Ident x} -> Just x
      _ -> Nothing

lowerIdent :: Parser Text
lowerIdent =
  lexeme
    $ token
    ?? mempty
    $ \case
      L.WithPos {value = L.Ident x} | isLower (T.head x) || T.head x == '_' -> Just x
      _ -> Nothing

upperIdent :: Parser Text
upperIdent =
  lexeme
    $ token
    ?? mempty
    $ \case
      L.WithPos {value = L.Ident x} | isUpper (T.head x) -> Just x
      _ -> Nothing

qualifiedIdent :: Parser (Text, Text)
qualifiedIdent =
  lexeme
    $ token
    ?? mempty
    $ \case
      L.WithPos {value = L.Qualified moduleName (L.Ident name)} -> Just (moduleName, name)
      _ -> Nothing

operator :: Parser Text
operator =
  lexeme
    $ token
    ?? mempty
    $ \case
      L.WithPos {value = L.Operator x} -> Just x
      _ -> Nothing

reserved :: L.ReservedId -> Parser ()
reserved r = lexeme $ void $ satisfy \case
  L.WithPos {value = L.ReservedId r'} -> r == r'
  _ -> False

reservedOp :: L.ReservedOp -> Parser ()
reservedOp r = lexeme $ void $ satisfy \case
  L.WithPos {value = L.ReservedOp r'} -> r == r'
  _ -> False

int :: (Num n) => Parser n
int =
  lexeme
    $ token
    ?? mempty
    $ \case
      L.WithPos {value = L.Int False x} -> Just $ fromInteger x
      _ -> Nothing

lexeme :: Parser a -> Parser a
lexeme m = do
  indentLevel <- ask
  -- skip (IndentStart n) or (IndentEnd n) if n is larger than the current indent level
  void $ takeWhileP Nothing \case
    L.WithPos {value = L.IndentStart n} -> n > indentLevel
    L.WithPos {value = L.IndentEnd n} -> n > indentLevel
    L.WithPos {value = L.Space _} -> True
    L.WithPos {value = L.Newlines} -> True
    _ -> False
  m

-- | Parse list of indented blocks that have the same indent level
blocks :: Parser a -> Parser [a]
blocks m = do
  (indentLevel, x) <- block m
  xs <- many $ block' indentLevel m
  pure (x : xs)
  where
    -- First block
    block m = label "block" do
      indentLevel <- indentStart
      local (const indentLevel) $ do
        x <- m
        indentEnd
        pure (indentLevel, x)
    -- Rest blocks
    block' indentLevel m = label ("continuous block " <> show indentLevel) $ try do
      indentLevel' <- indentStart
      if indentLevel == indentLevel'
        then local (const indentLevel') $ do
          x <- m
          indentEnd
          pure x
        else empty

indentStart :: Parser Int
indentStart = do
  L.WithPos {value = L.IndentStart n} <- satisfy \case
    L.WithPos {value = L.IndentStart _} -> True
    _ -> False
  pure n

indentEnd :: Parser ()
indentEnd = do
  indentLevel <- ask
  -- skip Indent* if n is larger than the current indent level using lexeme
  -- because we only need to parse a bracketed pair of IndentStart n and IndentEnd n
  void $ lexeme $ satisfy \case
    L.WithPos {value = L.IndentEnd n} -> assert (indentLevel == n) True
    _ -> False

pUnboxed :: Parser (Literal Unboxed)
pUnboxed = label "unboxed literal"
  $ lexeme
  $ token
  ?? mempty
  $ \case
    L.WithPos {value = L.Int True x} -> Just $ Int32 $ fromInteger x
    L.WithPos {value = L.Float True x} -> Just $ Double x
    L.WithPos {value = L.Char True x} -> Just $ Char x
    L.WithPos {value = L.String True x} -> Just $ String x
    _ -> Nothing

pBoxed :: Parser (Literal Boxed)
pBoxed = label "boxed literal"
  $ lexeme
  $ token
  ?? mempty
  $ \case
    L.WithPos {value = L.Int False x} -> Just $ Int32 $ fromInteger x
    L.WithPos {value = L.Float False x} -> Just $ Double x
    L.WithPos {value = L.Char False x} -> Just $ Char x
    L.WithPos {value = L.String False x} -> Just $ String x
    _ -> Nothing

asList :: Parser a -> Parser [a]
asList entry =
  try (entry `endBy1` reservedOp L.Semicolon)
    <|> try (entry `sepBy1` reservedOp L.Comma)
    <|> blocks entry