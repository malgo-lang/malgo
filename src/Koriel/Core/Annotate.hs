module Koriel.Core.Annotate (annotate) where

import Control.Lens (ifor)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Traversable (for)
import Effectful
import Effectful.Reader.Static
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Malgo.Prelude hiding (annotate)

-- | Type-check the program and annotate with type information.
annotate :: (IOE :> es) => ModuleName -> Program Text -> Eff es (Program (Meta Type))
annotate moduleName program = runReader (Context moduleName mempty) (annProgram program)

data Context = Context
  { -- | The current module name.
    moduleName :: ModuleName,
    nameEnv :: HashMap Text (Meta Type)
  }

lookupName :: (HasCallStack) => (Reader Context :> es) => Text -> Eff es (Meta Type)
lookupName name =
  HashMap.lookupDefault
    (error $ "lookupName: " <> show name)
    name
    <$> asks @Context (.nameEnv)

parseId :: (Reader Context :> es) => Text -> Type -> Eff es (Meta Type)
parseId name meta
  | T.head name == '@' = do
      case T.words (T.tail name) of
        [moduleName, name] ->
          pure Meta {meta, id = Id {name, moduleName = ModuleName moduleName, sort = External}}
        _ -> error "unreachable: parseId"
  | T.head name == '#' = do
      case T.words (T.tail name) of
        [moduleName, name, uniq] -> pure Meta {meta, id = Id {name = T.tail name, moduleName = ModuleName moduleName, sort = Internal (read $ convertString uniq)}}
        _ -> error "unreachable: parseId"
  | T.head name == '$' = do
      case T.words (T.tail name) of
        [moduleName, name, uniq] -> pure Meta {meta, id = Id {name = T.tail name, moduleName = ModuleName moduleName, sort = Temporal (read $ convertString uniq)}}
        _ -> error "unreachable: parseId"
  | T.head name == '%' = do
      moduleName <- asks @Context (.moduleName)
      pure Meta {meta, id = Id {name = T.tail name, moduleName, sort = Native}}
  | otherwise = do
      error $ "parseId: " <> show name

annProgram :: (Reader Context :> es, IOE :> es) => Program Text -> Eff es (Program (Meta Type))
annProgram Program {..} = do
  varEnv <- foldMapM prepareVarDecl topVars
  funEnv <- foldMapM prepareFunDecl topFuns
  local (\ctx -> ctx {nameEnv = varEnv <> funEnv}) do
    topVars <- traverse annVarDecl topVars
    topFuns <- traverse annFunDecl topFuns
    pure Program {..}

prepareVarDecl :: (Reader Context :> es) => (Text, Type, Expr Text) -> Eff es (HashMap Text (Meta Type))
prepareVarDecl (name, ty, _) = do
  id <- parseId name ty
  pure $ HashMap.singleton name id

prepareFunDecl :: (Reader Context :> es) => (Text, [Text], Type, Expr Text) -> Eff es (HashMap Text (Meta Type))
prepareFunDecl (name, _, ty, _) = do
  id <- parseId name ty
  pure $ HashMap.singleton name id

annVarDecl :: (Reader Context :> es, IOE :> es) => (Text, Type, Expr Text) -> Eff es (Meta Type, Type, Expr (Meta Type))
annVarDecl (name, ty, body) = do
  name <- lookupName name
  (name,ty,) <$> annExpr body

annFunDecl :: (Reader Context :> es, IOE :> es) => (Text, [Text], Type, Expr Text) -> Eff es (Meta Type, [Meta Type], Type, Expr (Meta Type))
annFunDecl (name, params, ty@(paramTypes :-> _), body) = do
  name <- lookupName name
  params' <- zipWithM parseId params paramTypes
  local
    (\ctx -> ctx {nameEnv = HashMap.fromList (zip params params') <> ctx.nameEnv})
    $ (name,params',ty,)
    <$> annExpr body
annFunDecl (name, _, _, _) = errorDoc $ "annFunDecl: " <> pretty name

annExpr :: (Reader Context :> es, IOE :> es) => Expr Text -> Eff es (Expr (Meta Type))
annExpr (Atom atom) = Atom <$> annAtom atom
annExpr (Call fun args) = Call <$> annAtom fun <*> traverse annAtom args
annExpr (CallDirect fun args) = CallDirect <$> lookupName fun <*> traverse annAtom args
annExpr (RawCall fun typ args) = RawCall fun typ <$> traverse annAtom args
annExpr (Cast typ x) = Cast typ <$> annAtom x
annExpr (Let defs body) = do
  (env, defs) <- foldMapM annDef defs
  body <- local (\ctx -> ctx {nameEnv = env <> ctx.nameEnv}) do
    annExpr body
  pure $ Let defs body
  where
    annDef (LocalDef variable typ object) = do
      variable' <- parseId variable typ
      object <- local (\ctx -> ctx {nameEnv = HashMap.insert variable variable' ctx.nameEnv}) do
        annObj typ object
      pure (HashMap.singleton variable variable', [LocalDef variable' typ object])
annExpr (Match scrutinee alts) = do
  scrutinee <- annExpr scrutinee
  Match scrutinee <$> traverse (annCase $ typeOf scrutinee) alts
  where
    annCase _ (Unpack (Con tag paramTypes) params body) = do
      params' <- zipWithM parseId params paramTypes
      local (\ctx -> ctx {nameEnv = HashMap.fromList (zip params params') <> ctx.nameEnv}) do
        body <- annExpr body
        pure $ Unpack (Con tag paramTypes) params' body
    annCase (RecordT fieldTypes) (OpenRecord fields body) = do
      fields' <- ifor fields \field variable -> do
        let ty = HashMap.lookupDefault (error $ "annExp: " <> show field) field fieldTypes
        parseId variable ty
      local (\ctx -> ctx {nameEnv = HashMap.fromList (HashMap.elems (HashMap.intersectionWith (,) fields fields')) <> ctx.nameEnv}) do
        body <- annExpr body
        pure $ OpenRecord fields' body
    annCase ty OpenRecord {} = error $ "annCase: " <> show ty
    annCase _ (Exact value body) = Exact value <$> annExpr body
    annCase _ (Bind var ty body) = do
      var' <- parseId var ty
      local (\ctx -> ctx {nameEnv = HashMap.insert var var' ctx.nameEnv}) do
        body <- annExpr body
        pure $ Bind var' ty body
annExpr (Switch v cases def) = Switch <$> annAtom v <*> traverse annCase cases <*> annExpr def
  where
    annCase (tag, body) = (tag,) <$> annExpr body
annExpr (SwitchUnboxed v cases def) = SwitchUnboxed <$> annAtom v <*> traverse annCase cases <*> annExpr def
  where
    annCase (tag, body) = (tag,) <$> annExpr body
annExpr (Destruct v con@(Con _ paramTypes) params body) = do
  v <- annAtom v
  params' <- zipWithM parseId params paramTypes
  local (\ctx -> ctx {nameEnv = HashMap.fromList (zip params params') <> ctx.nameEnv}) do
    body <- annExpr body
    pure $ Destruct v con params' body
annExpr (DestructRecord v kvs body) = do
  v <- annAtom v
  case typeOf v of
    RecordT kts -> do
      kvs' <-
        HashMap.fromList <$> for (HashMap.toList kvs) \(k, v) -> do
          let ty = HashMap.lookupDefault (error $ "annExp[DestructRecord]: " <> show k) k kts
          v' <- parseId v ty
          pure (k, v')
      local
        ( \ctx ->
            ctx
              { nameEnv =
                  HashMap.fromList (HashMap.elems (HashMap.intersectionWith (,) kvs kvs')) <> ctx.nameEnv
              }
        )
        do
          body <- annExpr body
          pure $ DestructRecord v kvs' body
    ty -> error $ "annExp[DestructRecord]: " <> show ty
annExpr (Assign x v e) = do
  v' <- annExpr v
  x' <- parseId x (typeOf v')
  local (\ctx -> ctx {nameEnv = HashMap.insert x x' ctx.nameEnv}) do
    e <- annExpr e
    pure $ Assign x' v' e
annExpr (Error ty) = pure $ Error ty

annAtom :: (HasCallStack) => (Reader Context :> es) => Atom Text -> Eff es (Atom (Meta Type))
annAtom (Var name) = Var <$> lookupName name
annAtom (Unboxed value) = pure $ Unboxed value

annObj :: (Reader Context :> es, IOE :> es) => Type -> Obj Text -> Eff es (Obj (Meta Type))
annObj (paramTypes :-> _) (Fun params body) = do
  params' <- zipWithM parseId params paramTypes
  local (\ctx -> ctx {nameEnv = HashMap.fromList (zip params params') <> ctx.nameEnv}) do
    body <- annExpr body
    pure $ Fun params' body
annObj ty Fun {} = error $ "annObj Fun: " <> show ty
annObj _ (Pack ty con args) = do
  Pack ty con <$> traverse annAtom args
annObj _ (Record fields) = Record <$> traverse annAtom fields
