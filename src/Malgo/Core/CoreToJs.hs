{-# LANGUAGE TemplateHaskell #-}

module Malgo.Core.CoreToJs where

import Control.Lens (At (at), makeLenses, use, (?=), (^.))
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Core.Syntax hiding (moduleName)
import Malgo.Prelude
import qualified Text.PrettyPrint as PP

type Arity = Int

data CodeGenEnv = CodeGenEnv
  { _moduleName :: ModuleName,
    _arityMap :: HashMap Name Arity
  }

makeLenses ''CodeGenEnv

lookupArity :: MonadState CodeGenEnv m => Id Type -> m (Maybe Int)
lookupArity x = use (arityMap . at x)

arityOf :: MonadState CodeGenEnv m => Exp -> m Int
arityOf (Var f) =
  lookupArity f >>= \case
    Just arity -> pure arity
    Nothing -> pure (-1) -- arityが分からない関数のarityは-1とする
arityOf (Apply f xs) = do
  fArity <- arityOf f
  pure $ fArity - length xs
arityOf (Fn ps _) = pure $ length ps
arityOf (Let _ _ e) = arityOf e
arityOf (Match _ _) =
  -- すべてのclauseのarityが同じなら、matchはarityをもつ
  -- TODO: ひとまず常に-1を返している
  pure $ -1
arityOf Unboxed {} = pure (-1)
arityOf Tuple {} = pure (-1)
arityOf Record {} = pure (-1)
arityOf RecordAccess {} = pure (-1)
arityOf Type {} = pure (-1)

codeGen :: (MonadReader env f, MonadIO f, HasUniqSupply env) => Module -> f Doc
codeGen Module {..} =
  evaluatingStateT CodeGenEnv {_moduleName = _moduleName, _arityMap = mempty} $ do
    typeDefs <- sep <$> traverse emitTypeDef _typeDefinitions
    traverse_ emitExtDef _externalDefinitions
    varDefs <- sep <$> traverse emitVarDef _variableDefinitions
    pure $ sep [typeDefs, varDefs]

emitVarDef :: (MonadReader env m, MonadState CodeGenEnv m) => (Id Type, Exp) -> m Doc
emitVarDef (name, exp) = do
  expArity <- arityOf exp
  arityMap . at name ?= expArity
  name <- emitVarName name
  exp <- runBlockBuilderT $ emitExp exp
  pure $ "const" <+> name <+> "=" <+> exp <> ";"

emitVarName :: (MonadReader env m, MonadState CodeGenEnv m) => Id a -> m Doc
emitVarName Id {_idName, _idUniq, _idSort = Internal} = pure $ text (zEncode $ toString _idName) <> "_" <> pPrint _idUniq
emitVarName Id {_idName, _idUniq, _idSort = External modName} = do
  currentModuleName <- use moduleName
  if modName == currentModuleName
    then pure $ text (zEncode $ toString _idName)
    else pure $ pPrint modName <> "." <> text (zEncode $ toString _idName)

zEncode :: String -> String
zEncode = concatMap encode'
  where
    encode' 'z' = "zz"
    encode' '#' = "zh"
    encode' c = [c]

emitExtDef :: MonadState CodeGenEnv m => (Id Type, String) -> m ()
emitExtDef (name, actual)
  | name ^. idName == toText actual = do
    let arity = arityOfExt (name ^. idMeta)
    -- Currently only supported `foreign import foo = "foo";`
    -- TODO: support `foreign import foo = { c "foo"; js "#1.foo()" };`
    arityMap . at name ?= arity
  | otherwise = error "not supported"
  where
    arityOfExt (TyFun _ t) = 1 + arityOfExt t
    arityOfExt _ = 0

{-
data Int32 = Int32# Int32#

const Int32# = (x) => ["Int32#", x];
-}
emitTypeDef :: (MonadReader env f, MonadState CodeGenEnv f, MonadIO f, HasUniqSupply env) => (a, TypeDef) -> f Doc
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
          pure $
            parens (sep $ punctuate "," $ reverse revArgs)
              <+> "=>"
              <+> brackets (sep $ punctuate "," $ doubleQuotes constrName : reverse revArgs)

emitExp :: (MonadReader env f, MonadState CodeGenEnv f) => Exp -> BlockBuilderT f Doc
emitExp (Var v) = emitVarName v
emitExp (Unboxed unboxed) = emitUnboxed unboxed
emitExp (Apply f xs) = do
  arityOfF <- arityOf f
  if arityOfF == length xs
    then do
      f <- emitExp f
      xs <- traverse emitExp xs
      pure $ parens f <> parens (sep $ punctuate "," xs)
    else error "partial application is not implemented"
emitExp (Fn params expr) = do
  params <- traverse emitVarName params
  expr <- runBlockBuilderT $ emitExp expr
  pure $ parens (sep $ punctuate "," params) <+> "=>" <+> expr
emitExp e = errorDoc $ pPrint e

emitUnboxed :: Applicative f => Unboxed -> f Doc
emitUnboxed (Int32 x) = pure $ pPrint (toInteger x)
emitUnboxed (Int64 x) = pure $ pPrint (toInteger x)
emitUnboxed (Float x) = pure $ pPrint x
emitUnboxed (Double x) = pure $ pPrint x
emitUnboxed (Char x) = pure $ quotes $ PP.char x
emitUnboxed (String x) = pure $ doubleQuotes $ PP.text (toString x)

type BlockBuilderT m a = WriterT (Endo [Doc]) m a

runBlockBuilderT :: Monad m => WriterT (Endo [Doc]) m Doc -> m Doc
runBlockBuilderT m = do
  (result, binds) <- runWriterT m
  case appEndo binds [] of
    [] -> pure result
    binds ->
      pure $
        braces $
          sep (punctuate ";" binds) <> ";" $$ "return" <+> result <> ";"
