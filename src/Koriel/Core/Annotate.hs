module Koriel.Core.Annotate (annotate) where

import Control.Lens (ifor)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.Prelude
import Koriel.Pretty (Pretty (pPrint), errorDoc)

-- | Type-check the program and annotate with type information.
annotate :: MonadIO m => ModuleName -> Program Text -> m (Program (Id Type))
annotate moduleName program = runReaderT (annProgram program) (Context moduleName mempty)

data Context = Context
  { -- | The current module name.
    moduleName :: ModuleName,
    nameEnv :: HashMap Text (Id Type)
  }

lookupName :: HasCallStack => MonadReader Context m => Text -> m (Id Type)
lookupName name =
  HashMap.lookupDefault
    (error $ "lookupName: " <> show name)
    name
    <$> asks (.nameEnv)

parseId :: MonadReader Context m => Text -> Type -> m (Id Type)
parseId name meta
  | Text.head name == '@' = do
      (moduleName, name) <- pure $ second Text.tail $ Text.breakOn "." (Text.tail name)
      pure Id {name, meta, moduleName = ModuleName moduleName, sort = External}
  | Text.head name == '#' = do
      moduleName <- asks (.moduleName)
      pure Id {name = Text.tail name, meta, moduleName, sort = Internal}
  | Text.head name == '$' = do
      moduleName <- asks (.moduleName)
      pure Id {name = Text.tail name, meta, moduleName, sort = Temporal}
  | Text.head name == '%' = do
      moduleName <- asks (.moduleName)
      pure Id {name = Text.tail name, meta, moduleName, sort = Native}
  | otherwise = do
      error $ "parseId: " <> show name

annProgram :: (MonadReader Context m, MonadIO m) => Program Text -> m (Program (Id Type))
annProgram Program {..} = do
  varEnv <- foldMapM prepareVarDecl topVars
  funEnv <- foldMapM prepareFunDecl topFuns
  local (\ctx -> ctx {nameEnv = varEnv <> funEnv}) do
    topVars <- traverse annVarDecl topVars
    topFuns <- traverse annFunDecl topFuns
    pure Program {..}

prepareVarDecl :: MonadReader Context m => (Text, Type, Exp Text) -> m (HashMap Text (Id Type))
prepareVarDecl (name, ty, _) = do
  id <- parseId name ty
  pure $ one (name, id)

prepareFunDecl :: MonadReader Context m => (Text, [Text], Type, Exp Text) -> m (HashMap Text (Id Type))
prepareFunDecl (name, _, ty, _) = do
  id <- parseId name ty
  pure $ one (name, id)

annVarDecl :: (MonadReader Context m, MonadIO m) => (Text, Type, Exp Text) -> m (Id Type, Type, Exp (Id Type))
annVarDecl (name, ty, body) = do
  name <- lookupName name
  (name,ty,) <$> annExp body

annFunDecl :: (MonadReader Context m, MonadIO m) => (Text, [Text], Type, Exp Text) -> m (Id Type, [Id Type], Type, Exp (Id Type))
annFunDecl (name, params, ty@(paramTypes :-> _), body) = do
  name <- lookupName name
  params' <- zipWithM parseId params paramTypes
  local
    (\ctx -> ctx {nameEnv = HashMap.fromList (zip params params') <> ctx.nameEnv})
    $ (name,params',ty,) <$> annExp body
annFunDecl (name, _, _, _) = errorDoc $ "annFunDecl: " <> pPrint name

annExp :: (MonadReader Context m, MonadIO m) => Exp Text -> m (Exp (Id Type))
annExp (Atom atom) = Atom <$> annAtom atom
annExp (Call fun args) = Call <$> annAtom fun <*> traverse annAtom args
annExp (CallDirect fun args) = CallDirect <$> lookupName fun <*> traverse annAtom args
annExp (RawCall fun typ args) = RawCall fun typ <$> traverse annAtom args
annExp (BinOp op x y) = BinOp op <$> annAtom x <*> annAtom y
annExp (Cast typ x) = Cast typ <$> annAtom x
annExp (Let defs body) = do
  (env, defs) <- foldMapM annDef defs
  body <- local (\ctx -> ctx {nameEnv = env <> ctx.nameEnv}) do
    annExp body
  pure $ Let defs body
  where
    annDef (LocalDef variable typ object) = do
      variable' <- parseId variable typ
      object <- local (\ctx -> ctx {nameEnv = HashMap.insert variable variable' ctx.nameEnv}) do
        annObj typ object
      pure (one (variable, variable'), [LocalDef variable' typ object])
annExp (Match scrutinee alts) = do
  scrutinee <- annExp scrutinee
  Match scrutinee <$> traverse (annCase $ typeOf scrutinee) alts
  where
    annCase _ (Unpack (Con tag paramTypes) params body) = do
      params' <- zipWithM parseId params paramTypes
      local (\ctx -> ctx {nameEnv = HashMap.fromList (zip params params') <> ctx.nameEnv}) do
        body <- annExp body
        pure $ Unpack (Con tag paramTypes) params' body
    annCase (RecordT fieldTypes) (OpenRecord fields body) = do
      fields' <- ifor fields \field variable -> do
        let ty = HashMap.lookupDefault (error $ "annExp: " <> show field) field fieldTypes
        parseId variable ty
      local (\ctx -> ctx {nameEnv = HashMap.fromList (HashMap.elems (HashMap.intersectionWith (,) fields fields')) <> ctx.nameEnv}) do
        body <- annExp body
        pure $ OpenRecord fields' body
    annCase ty OpenRecord {} = error $ "annCase: " <> show ty
    annCase _ (Exact value body) = Exact value <$> annExp body
    annCase _ (Bind var ty body) = do
      var' <- parseId var ty
      local (\ctx -> ctx {nameEnv = HashMap.insert var var' ctx.nameEnv}) do
        body <- annExp body
        pure $ Bind var' ty body
annExp (Switch v cases) = Switch <$> annAtom v <*> traverse annCase cases
  where
    annCase (tag, body) = (tag,) <$> annExp body
annExp (Destruct v con@(Con _ paramTypes) params body) = do
  v <- annAtom v
  params' <- zipWithM parseId params paramTypes
  local (\ctx -> ctx {nameEnv = HashMap.fromList (zip params params') <> ctx.nameEnv}) do
    body <- annExp body
    pure $ Destruct v con params' body
annExp (Error ty) = pure $ Error ty

annAtom :: HasCallStack => MonadReader Context m => Atom Text -> m (Atom (Id Type))
annAtom (Var name) = Var <$> lookupName name
annAtom (Unboxed value) = pure $ Unboxed value

annObj :: (MonadReader Context m, MonadIO m) => Type -> Obj Text -> m (Obj (Id Type))
annObj (paramTypes :-> _) (Fun params body) = do
  params' <- zipWithM parseId params paramTypes
  local (\ctx -> ctx {nameEnv = HashMap.fromList (zip params params') <> ctx.nameEnv}) do
    body <- annExp body
    pure $ Fun params' body
annObj ty Fun {} = error $ "annObj Fun: " <> show ty
annObj _ (Pack ty con args) = do
  Pack ty con <$> traverse annAtom args
annObj _ (Record fields) = Record <$> traverse annAtom fields