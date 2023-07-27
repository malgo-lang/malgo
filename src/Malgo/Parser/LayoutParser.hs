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

type Parser = ReaderT Int (Parsec Void LexStream)

parse :: String -> LexStream -> Either (ParseErrorBundle LexStream Void) (Module (Malgo Parse))
parse =
  M.parse
    ( runReaderT ?? 0 $ do
        m <- pModule
        eof
        pure m
    )

pModule :: Parser (Module (Malgo Parse))
pModule = do
  reserved L.Module
  name <- pModuleName
  reservedOp L.Equal
  decls <- pDecls
  pure Module {moduleName = name, moduleDefinition = ParsedDefinitions decls}
{-# INLINEABLE pModule #-}

pModuleName :: Parser ModuleName
pModuleName = ModuleName <$> ident
{-# INLINE pModuleName #-}

pDecls :: Parser [Decl (Malgo Parse)]
pDecls = between (reservedOp L.LBrace) (reservedOp L.RBrace) $ blocks pDecl
{-# INLINE pDecls #-}

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
{-# INLINE pDecl #-}

pScDef :: Parser (Decl (Malgo Parse))
pScDef = do
  start <- getSourcePos
  reserved L.Def
  name <- lowerIdent <|> between (reservedOp L.LParen) (reservedOp L.RParen) operator
  reservedOp L.Equal
  body <- pExpr
  end <- getSourcePos
  pure $ ScDef (Range start end) name body
{-# INLINEABLE pScDef #-}

pExpr :: Parser (Expr (Malgo Parse))
pExpr = do
  start <- getSourcePos
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
    pBrace = pFun <|> pRecord
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
      xs <- asList pExpr
      end <- getSourcePos
      pure $ List (Range start end) xs
    pAnnotation start expr = do
      reservedOp L.Colon
      ty <- pType
      end <- getSourcePos
      pure $ Ann (Range start end) expr ty
{-# INLINE pExpr #-}

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
      pat <- try (some pSinglePat <* reservedOp L.Arrow) <|> pure []
      stmts <- do
        start <- getSourcePos
        stmts <- pStmts
        end <- getSourcePos
        pure $ Seq (Range start end) stmts
      end <- getSourcePos
      pure $ Clause (Range start end) pat stmts
    pStmts = undefined
    pSinglePat :: Parser (Pat (Malgo Parse))
    pSinglePat =
      choice [pVarP, pConP, try pUnboxedP, pBoxedP, pParenP, pRecordP, pListP]
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
          kvs <- between (reservedOp L.LBrace) (reservedOp L.RBrace) do
            asList pRecordPEntry
          end <- getSourcePos
          pure $ RecordP (Range start end) kvs
          where
            pRecordPEntry = do
              label <- lowerIdent
              reservedOp L.Equal
              pat <- pPat
              pure (label, pat)
        pListP = do
          start <- getSourcePos
          xs <- between (reservedOp L.LBracket) (reservedOp L.RBracket) $ asList pPat
          end <- getSourcePos
          pure $ ListP (Range start end) xs
    pPat = do
      try pConP <|> pSinglePat
      where
        pConP = do
          start <- getSourcePos
          name <- upperIdent
          args <- some pSinglePat
          end <- getSourcePos
          pure $ ConP (Range start end) name args

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

pScSig :: Parser (Decl (Malgo Parse))
pScSig = do
  start <- getSourcePos
  reserved L.Def
  name <- lowerIdent <|> between (reservedOp L.LParen) (reservedOp L.RParen) operator
  reservedOp L.Colon
  ty <- pType
  end <- getSourcePos
  pure $ ScSig (Range start end) name ty
{-# INLINEABLE pScSig #-}

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
{-# INLINE pType #-}

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
{-# INLINE pSingleType #-}

asList :: Parser a -> Parser [a]
asList entry =
  try (entry `endBy1` reservedOp L.Semicolon)
    <|> try (entry `sepBy1` reservedOp L.Comma)
    <|> blocks entry
{-# INLINE asList #-}

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
{-# INLINEABLE pDataDef #-}

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
{-# INLINEABLE pTypeSynonym #-}

pInfix :: Parser (Decl (Malgo Parse))
pInfix = label "infix" do
  start <- getSourcePos
  assoc <- choice [reserved L.Infixl $> LeftA, reserved L.Infixr $> RightA, reserved L.Infix $> NeutralA]
  i <- int
  name <- between (reservedOp L.LParen) (reservedOp L.RParen) operator
  end <- getSourcePos
  pure $ Infix (Range start end) assoc i name
{-# INLINEABLE pInfix #-}

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
{-# INLINEABLE pForeign #-}

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
    {-# INLINE pImportList #-}
    importPartial = do
      between (reservedOp L.LBrace) (reservedOp L.RBrace)
        $ importAll
        <|> importSelected
    {-# INLINE importPartial #-}
    importAll = do
      reservedOp L.DotDot
      pure All
    {-# INLINE importAll #-}
    importSelected = do
      Selected <$> choice [lowerIdent, upperIdent, between (reservedOp L.LParen) (reservedOp L.RParen) operator] `sepBy` reservedOp L.Comma
    {-# INLINE importSelected #-}
    importPrefix = As <$> pModuleName
    {-# INLINE importPrefix #-}
{-# INLINEABLE pImport #-}

-- * common combinators

ident :: Parser Text
ident =
  lexeme
    $ token
    ?? mempty
    $ \case
      L.WithPos {value = L.Ident x} -> Just x
      _ -> Nothing
{-# INLINE ident #-}

lowerIdent :: Parser Text
lowerIdent =
  lexeme
    $ token
    ?? mempty
    $ \case
      L.WithPos {value = L.Ident x} | isLower (T.head x) -> Just x
      _ -> Nothing
{-# INLINE lowerIdent #-}

upperIdent :: Parser Text
upperIdent =
  lexeme
    $ token
    ?? mempty
    $ \case
      L.WithPos {value = L.Ident x} | isUpper (T.head x) -> Just x
      _ -> Nothing
{-# INLINE upperIdent #-}

qualifiedIdent :: Parser (Text, Text)
qualifiedIdent =
  lexeme
    $ token
    ?? mempty
    $ \case
      L.WithPos {value = L.Qualified moduleName (L.Ident name)} -> Just (moduleName, name)
      _ -> Nothing
{-# INLINE qualifiedIdent #-}

operator :: Parser Text
operator =
  lexeme
    $ token
    ?? mempty
    $ \case
      L.WithPos {value = L.Operator x} -> Just x
      _ -> Nothing
{-# INLINE operator #-}

reserved :: L.ReservedId -> Parser ()
reserved r = lexeme $ void $ satisfy \case
  L.WithPos {value = L.ReservedId r'} -> r == r'
  _ -> False
{-# INLINE reserved #-}

reservedOp :: L.ReservedOp -> Parser ()
reservedOp r = lexeme $ void $ satisfy \case
  L.WithPos {value = L.ReservedOp r'} -> r == r'
  _ -> False
{-# INLINE reservedOp #-}

int :: (Num n) => Parser n
int =
  lexeme
    $ token
    ?? mempty
    $ \case
      L.WithPos {value = L.Int False x} -> Just $ fromInteger x
      _ -> Nothing
{-# INLINE int #-}

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
{-# INLINE lexeme #-}

-- | Parse list of indented blocks that have the same indent level
blocks :: Parser a -> Parser [a]
blocks m = do
  (indentLevel, x) <- block m
  xs <- many $ block' indentLevel m
  pure (x : xs)
  where
    -- First block
    block m = do
      indentLevel <- indentStart
      local (const indentLevel) $ do
        x <- m
        indentEnd
        pure (indentLevel, x)
    -- Rest blocks
    block' indentLevel m = try do
      indentLevel' <- indentStart
      if indentLevel == indentLevel'
        then local (const indentLevel') $ do
          x <- m
          indentEnd
          pure x
        else empty
{-# INLINE blocks #-}

indentStart :: Parser Int
indentStart = do
  L.WithPos {value = L.IndentStart n} <- satisfy \case
    L.WithPos {value = L.IndentStart _} -> True
    _ -> False
  pure n
{-# INLINE indentStart #-}

indentEnd :: Parser ()
indentEnd = do
  indentLevel <- ask
  -- skip Indent* if n is larger than the current indent level using lexeme
  -- because we only need to parse a bracketed pair of IndentStart n and IndentEnd n
  void $ lexeme $ satisfy \case
    L.WithPos {value = L.IndentEnd n} -> assert (indentLevel == n) True
    _ -> False
{-# INLINE indentEnd #-}