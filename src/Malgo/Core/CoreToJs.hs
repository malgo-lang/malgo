{-# LANGUAGE TemplateHaskell #-}

module Malgo.Core.CoreToJs where

import Control.Lens (At (at), makeLenses, use, (?=))
import Debug.Pretty.Simple (pTraceShowM)
import Koriel.Id
import Koriel.Pretty
import Malgo.Core.Syntax hiding (moduleName)
import Malgo.Prelude

type Arity = Int

data CodeGenEnv = CodeGenEnv
  { _moduleName :: ModuleName,
    _arityMap :: HashMap Name Arity
  }

makeLenses ''CodeGenEnv

lookupArity x = use (arityMap . at x)

arityOf (Var f) =
  lookupArity f >>= \case
    Just arity -> pure arity
    Nothing -> pure (-1) -- arityが分からない関数のarityは-1とする

codeGen Module {..} =
  evaluatingStateT CodeGenEnv {_moduleName = _moduleName, _arityMap = mempty} $ do
    typeDefs <- sep <$> traverse emitTypeDef _typeDefinitions
    varDefs <- sep <$> traverse emitVarDef _variableDefinitions
    pure $ sep [typeDefs, varDefs]

emitVarDef (name, exp) = do
  name <- emitVarName name
  exp <- emitExp exp
  pure $ "const" <+> name <+> "=" <+> exp <> ";"

emitVarName :: (MonadReader env m, MonadState CodeGenEnv m) => Id a -> m Doc
emitVarName Id {_idName, _idUniq, _idSort = Internal} = pure $ text (zEncode $ toString _idName) <> "_" <> pPrint _idUniq
emitVarName Id {_idName, _idUniq, _idSort = External modName} = do
  currentModuleName <- use moduleName
  if modName == currentModuleName
    then pure $ text (zEncode $ toString _idName)
    else pure $ pPrint modName <> "." <> text (zEncode $ toString _idName)

zEncode = concatMap encode'
  where
    encode' 'z' = "zz"
    encode' '#' = "zh"
    encode' c = [c]

{-
data Int32 = Int32# Int32#

const Int32# = (x) => ["Int32#", x];
-}
emitTypeDef (_, TypeDef {..}) = sep <$> traverse emitConstr _constructors
  where
    emitConstr constr@Id {..} = do
      constrName <- emitVarName constr
      constrFunc <- buildConstrFunc constrName [] _idMeta
      pure $ "const" <+> constrName <+> "=" <+> constrFunc <> ";"
        where
          buildConstrFunc constrName revArgs (TyFun _ t2) = do
            x <- emitVarName =<< newInternalId "x" ()
            buildConstrFunc constrName (x : revArgs) t2
          buildConstrFunc constrName revArgs _ = do
            arityMap . at constr ?= length revArgs
            pure $ parens (sep $ punctuate "," $ reverse revArgs) <+> "=>" <+> brackets (sep $ punctuate "," $ constrName : reverse revArgs)

emitExp (Var v) = emitVarName v
emitExp (Unboxed unboxed) = emitUnboxed unboxed
emitExp (Apply f xs) = do
  pTraceShowM f
  arityOfF <- arityOf f
  if arityOfF == length xs
    then do
      f <- emitExp f
      xs <- traverse emitExp xs
      pure $ parens f <> parens (sep $ punctuate "," xs)
    else error "partial application is not implemented"

emitUnboxed (Int32 x) = pure $ pPrint (toInteger x)
