module Koriel.Core.Annotate (annotate) where

import Control.Lens (ifor)
import Data.HashMap.Strict qualified as HashMap
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.Prelude

-- | Type-check the program and annotate with type information.
annotate :: MonadIO m => ModuleName -> Program Text -> m (Program (Id Type))
annotate moduleName program = runReaderT (annProgram program) (Context moduleName mempty)

data Context = Context
  { -- | The current module name.
    moduleName :: ModuleName,
    -- | Textual identifiers to unique identifiers.
    nameEnv :: HashMap Text (Id Type)
  }

lookupName :: MonadReader Context m => Text -> m (Id Type)
lookupName name =
  HashMap.lookupDefault
    (error $ "lookupName: " <> show name)
    name
    <$> asks (.nameEnv)

annProgram :: (MonadReader Context m, MonadIO m) => Program Text -> m (Program (Id Type))
annProgram Program {..} = do
  varEnv <- foldMapM prepareVarDecl topVars
  funEnv <- foldMapM prepareFunDecl topFuns
  extEnv <- foldMapM prepareExtDecl extFuns
  local (\ctx -> ctx {nameEnv = varEnv <> funEnv <> extEnv}) do
    topVars <- traverse annVarDecl topVars
    topFuns <- traverse annFunDecl topFuns
    pure Program {..}

prepareVarDecl :: MonadReader Context m => (Text, Type, Exp Text) -> m (HashMap Text (Id Type))
prepareVarDecl (name, ty, _) = do
  id <- newExternalId name ty
  pure $ one (name, id)

prepareFunDecl :: MonadReader Context m => (Text, [Text], Type, Exp Text) -> m (HashMap Text (Id Type))
prepareFunDecl (name, _, ty, _) = do
  id <- newExternalId name ty
  pure $ one (name, id)

prepareExtDecl :: MonadReader Context m => (Text, Type) -> m (HashMap Text (Id Type))
prepareExtDecl (name, ty) = do
  id <- newExternalId name ty
  pure $ one (name, id)

annVarDecl :: (MonadReader Context m, MonadIO m) => (Text, Type, Exp Text) -> m (Id Type, Type, Exp (Id Type))
annVarDecl (name, ty, body) = do
  name <- lookupName name
  (name,ty,) <$> annExp body

annFunDecl :: (MonadReader Context m, MonadIO m) => (Text, [Text], Type, Exp Text) -> m (Id Type, [Id Type], Type, Exp (Id Type))
annFunDecl (name, args, ty, body) = do
  name <- lookupName name
  args <- traverse lookupName args
  (name,args,ty,) <$> annExp body

annExp :: (MonadReader Context m, MonadIO m) => Exp Text -> m (Exp (Id Type))
annExp (Atom atom) = Atom <$> annAtom atom
annExp (Call fun args) = Call <$> annAtom fun <*> traverse annAtom args
annExp (CallDirect fun args) = CallDirect <$> lookupName fun <*> traverse annAtom args
annExp (RawCall fun typ args) = RawCall fun typ <$> traverse annAtom args
annExp (BinOp op x y) = BinOp op <$> annAtom x <*> annAtom y
annExp (Cast typ x) = Cast typ <$> annAtom x
annExp (Let defs body) = do
  defs <- traverse annDef defs
  body <- annExp body
  pure $ Let defs body
  where
    annDef (LocalDef variable typ object) = do
      variable <- lookupName variable
      object <- annObj typ object
      pure $ LocalDef variable typ object
annExp (Match scrutinee alts) = do
  scrutinee <- annExp scrutinee
  Match scrutinee <$> traverse (annCase $ typeOf scrutinee) alts
  where
    annCase _ (Unpack (Con tag paramTypes) params body) = do
      params' <- zipWithM newInternalId' params paramTypes
      local (\ctx -> ctx {nameEnv = HashMap.fromList (zip params params') <> ctx.nameEnv}) do
        body <- annExp body
        pure $ Unpack (Con tag paramTypes) params' body
    annCase (RecordT fieldTypes) (OpenRecord fields body) = do
      fields' <- ifor fields \field variable -> do
        let ty = HashMap.lookupDefault (error $ "annExp: " <> show field) field fieldTypes
        newInternalId' variable ty
      local (\ctx -> ctx {nameEnv = HashMap.fromList (HashMap.elems (HashMap.intersectionWith (,) fields fields')) <> ctx.nameEnv}) do
        body <- annExp body
        pure $ OpenRecord fields' body
    annCase ty OpenRecord {} = error $ "annCase: " <> show ty
    annCase _ (Switch value body) = Switch value <$> annExp body
    annCase _ (Bind var ty body) = do
      var' <- newInternalId' var ty
      local (\ctx -> ctx {nameEnv = HashMap.insert var var' ctx.nameEnv}) do
        body <- annExp body
        pure $ Bind var' ty body
annExp (Error ty) = pure $ Error ty

annAtom :: MonadReader Context m => Atom Text -> m (Atom (Id Type))
annAtom (Var name) = Var <$> lookupName name
annAtom (Unboxed value) = pure $ Unboxed value

annObj :: (MonadReader Context m, MonadIO m) => Type -> Obj Text -> m (Obj (Id Type))
annObj (paramTypes :-> _) (Fun params body) = do
  params' <- zipWithM newInternalId' params paramTypes
  local (\ctx -> ctx {nameEnv = HashMap.fromList (zip params params') <> ctx.nameEnv}) do
    body <- annExp body
    pure $ Fun params' body
annObj ty Fun {} = error $ "annObj Fun: " <> show ty
annObj _ (Pack ty con args) = do
  Pack ty con <$> traverse annAtom args
annObj _ (Record fields) = Record <$> traverse annAtom fields