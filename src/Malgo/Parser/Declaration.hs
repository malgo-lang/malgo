{-# LANGUAGE NoMonomorphismRestriction #-}

module Malgo.Parser.Declaration
  ( pModuleFile,
    pDecl,
  )
where

import Control.Monad.Trans (lift)
import Effectful (IOE, (:>))
import Malgo.Features
import Malgo.Module (ModuleName (..), Workspace, parseArtifactPath, pwdPath)
import Malgo.Parser.Common
import Malgo.Parser.Expression (pExpr)
import Malgo.Parser.Lexer
import Malgo.Parser.Type (pAtomType, pType)
import Malgo.Prelude hiding (All)
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec

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

pDecl :: (IOE :> es, Workspace :> es, Features :> es) => Parser es (Decl (Malgo Parse))
pDecl = do
  _ <- many skipPragma
  choice
    [ pDataDef,
      pTypeSynonym,
      pInfix,
      pForeign,
      pImport,
      try pScSig,
      pScDef
    ]

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

pScSig :: Parser es (Decl (Malgo Parse))
pScSig = do
  start <- getSourcePos
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator ":"
  ty <- pType
  end <- getSourcePos
  pure $ ScSig (Range start end) name ty

pScDef :: (Features :> es) => Parser es (Decl (Malgo Parse))
pScDef = do
  start <- getSourcePos
  reserved "def"
  name <- choice [ident, between (symbol "(") (symbol ")") operator]
  reservedOperator "="
  body <- pExpr
  end <- getSourcePos
  pure $ ScDef (Range start end) name body