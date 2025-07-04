{-# LANGUAGE NoMonomorphismRestriction #-}

module Malgo.NewParser.Declaration
  ( pModuleFile,
    pDecl,
  )
where

import Effectful (IOE, (:>))
import Control.Monad.Trans (lift)
import Malgo.Features
import Malgo.Module (ModuleName (..), Workspace, parseArtifactPath, pwdPath)
import Malgo.NewParser.Common
import Malgo.NewParser.Expression (pExpr)
import Malgo.NewParser.Lexer
import Malgo.NewParser.Type (pAtomType, pType)
import Malgo.Prelude hiding (All)
import Malgo.Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec hiding (optional)

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

-- NEW SYNTAX: data List(a) = Cons(a, List(a))
pDataDef :: (Features :> es) => Parser es (Decl (Malgo Parse))
pDataDef = do
  start <- getSourcePos
  reserved "data"
  name <- ident
  parameters <- 
    optional (between (symbol "(") (symbol ")") $ sepBy1 pTypeParam (symbol ",")) >>= \case
      Just params -> pure params
      Nothing -> pure []
  reservedOperator "="
  constructors <- sepBy1 pConstructor (reservedOperator "|")
  end <- getSourcePos
  pure $ DataDef (Range start end) name parameters constructors
  where
    pTypeParam = do
      start <- getSourcePos
      parameter <- ident
      end <- getSourcePos
      pure (Range start end, parameter)
    
    -- NEW SYNTAX: Cons(a, List(a))
    pConstructor = do
      start <- getSourcePos
      name <- ident
      parameters <- 
        optional (between (symbol "(") (symbol ")") $ sepBy pAtomType (symbol ",")) >>= \case
          Just params -> pure params
          Nothing -> pure []
      end <- getSourcePos
      pure (Range start end, name, parameters)

pTypeSynonym :: (Features :> es) => Parser es (Decl (Malgo Parse))
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

pForeign :: (Features :> es) => Parser es (Decl (Malgo Parse))
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
  pattern <- pModulePattern
  reservedOperator "="
  reserved "import"
  path <- pStringLiteral
  end <- getSourcePos
  pure $ Import (Range start end) (ModuleName path) pattern
  where
    pModulePattern = choice
      [ do
          symbol "{"
          symbol ".."
          symbol "}"
          pure All,
        do
          idents <- between (symbol "{") (symbol "}") $ sepBy1 ident (symbol ",")
          pure $ Selected idents,
        As <$> pModuleName
      ]
    
    pModuleName = ModuleName <$> ident

pScSig :: (Features :> es) => Parser es (Decl (Malgo Parse))
pScSig = do
  start <- getSourcePos
  reserved "def"
  name <- choice
    [ ident,
      between (symbol "(") (symbol ")") operator
    ]
  reservedOperator ":"
  ty <- pType
  end <- getSourcePos
  pure $ ScSig (Range start end) name ty

pScDef :: (Features :> es) => Parser es (Decl (Malgo Parse))
pScDef = do
  start <- getSourcePos
  reserved "def"
  name <- choice
    [ ident,
      between (symbol "(") (symbol ")") operator
    ]
  reservedOperator "="
  body <- pExpr
  end <- getSourcePos
  pure $ ScDef (Range start end) name body