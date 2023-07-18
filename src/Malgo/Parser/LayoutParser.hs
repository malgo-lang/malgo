module Malgo.Parser.LayoutParser (parse) where

import Control.Exception (assert)
import Control.Monad.Reader
import Data.Text qualified as T
import Koriel.Id
import Malgo.Parser.Stream (LexStream)
import Malgo.Parser.Stream qualified as L
import Malgo.Prelude
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
{-# INLINE pModule #-}

pModuleName :: Parser ModuleName
pModuleName = ModuleName . (.value) <$> ident
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
  pure $ ScDef (Range start end) name.value body
{-# INLINE pScDef #-}

pExpr :: Parser (Expr (Malgo Parse))
pExpr = _
{-# INLINE pExpr #-}

pScSig :: Parser (Decl (Malgo Parse))
pScSig = do
  start <- getSourcePos
  reserved L.Def
  name <- lowerIdent <|> between (reservedOp L.LParen) (reservedOp L.RParen) operator
  reservedOp L.Colon
  ty <- pType
  end <- getSourcePos
  pure $ ScSig (Range start end) name.value ty
{-# INLINE pScSig #-}

pType :: Parser (Type (Malgo Parse))
pType = _
{-# INLINE pType #-}

pDataDef :: Parser (Decl (Malgo Parse))
pDataDef = do
  start <- getSourcePos
  reserved L.Data
  name <- upperIdent
  args <- many do
    start <- getSourcePos
    arg <- lowerIdent
    end <- getSourcePos
    pure (Range start end, arg.value)
  reservedOp L.Equal
  cons <- blocks pConDef
  end <- getSourcePos
  pure $ DataDef (Range start end) name.value args cons
  where
    pConDef = do
      start <- getSourcePos
      name <- upperIdent
      params <- many pSingleType
      end <- getSourcePos
      pure (Range start end, name.value, params)
{-# INLINE pDataDef #-}

pSingleType :: Parser (Type (Malgo Parse))
pSingleType = _
{-# INLINE pSingleType #-}

-- * common combinators

ident :: Parser (L.WithPos Text)
ident =
  lexeme
    $ satisfy \case
      L.WithPos {value = L.Ident _} -> True
      _ -> False
    <&> \case
      p@L.WithPos {value = L.Ident x} -> p {L.value = x}
      _ -> error "impossible"
{-# INLINE ident #-}

lowerIdent :: Parser (L.WithPos Text)
lowerIdent =
  lexeme
    $ satisfy \case
      L.WithPos {value = L.Ident x} -> isLower (T.head x)
      _ -> False
    <&> \case
      p@L.WithPos {value = L.Ident x} -> p {L.value = x}
      _ -> error "impossible"
{-# INLINE lowerIdent #-}

upperIdent :: Parser (L.WithPos Text)
upperIdent =
  lexeme
    $ satisfy \case
      L.WithPos {value = L.Ident x} -> isUpper (T.head x)
      _ -> False
    <&> \case
      p@L.WithPos {value = L.Ident x} -> p {L.value = x}
      _ -> error "impossible"
{-# INLINE upperIdent #-}

operator :: Parser (L.WithPos Text)
operator =
  lexeme
    $ satisfy \case
      L.WithPos {value = L.Operator _} -> True
      _ -> False
    <&> \case
      p@L.WithPos {value = L.Operator x} -> p {L.value = x}
      _ -> error "impossible"
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

lexeme :: Parser a -> Parser a
lexeme m = do
  indentLevel <- ask
  -- skip (IndentStart n) or (IndentEnd n) if n is larger than the current indent level
  void $ takeWhileP (Just "deep indent") \case
    L.WithPos {value = L.IndentStart n} -> n > indentLevel
    L.WithPos {value = L.IndentEnd n} -> n > indentLevel
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
