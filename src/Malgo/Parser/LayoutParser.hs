module Malgo.Parser.LayoutParser (parse) where

import Control.Exception (assert)
import Control.Monad.Reader
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
pExpr = undefined
{-# INLINE pExpr #-}

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
pType = undefined
{-# INLINE pType #-}

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

pSingleType :: Parser (Type (Malgo Parse))
pSingleType = undefined
{-# INLINE pSingleType #-}

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