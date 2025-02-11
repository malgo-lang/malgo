module Malgo.Core.Eval (eval, EvalError) where

import Data.Map qualified as Map
import Data.Traversable (for)
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local
import Malgo.Core.Syntax
import Malgo.Core.Type
import Malgo.Id
import Malgo.Module
import Malgo.MonadUniq (Uniq)
import Malgo.Prelude hiding (catchError, lookup, throwError)

eval :: (IOE :> es, Error EvalError :> es, Reader ModuleName :> es, State Uniq :> es) => Program Name -> Eff es ()
eval program = evalState @Env mempty do
  traverse_ evalTopFun program.topFuns
  traverse_ initTopVar program.topVars
  traverse_ evalTopVar program.topVars
  main <- findMain program.topFuns
  void $ evalExpr (CallDirect main [])
  where
    findMain [] = throwError NoMain
    findMain ((name, _, _, _) : rest)
      | name.id.name == "main" = pure name
      | otherwise = findMain rest

evalTopFun :: (State Env :> es, IOE :> es) => (Name, [Name], c, Expr Name) -> Eff es ()
evalTopFun (name, parameters, _, body) = do
  ref <- newRef name
  -- Top-level functions are not closures. So we don't need to capture the environment.
  writeRef ref (VFun Nothing parameters body)
  assign [(name, ref)]

initTopVar :: (State Env :> es, IOE :> es) => (Name, b, c) -> Eff es ()
initTopVar (name, _, _) = do
  ref <- newRef name
  modify (Map.insert name ref)

evalTopVar :: (State Env :> es, Error EvalError :> es, IOE :> es, State Uniq :> es, Reader ModuleName :> es) => (Name, b, Expr Name) -> Eff es ()
evalTopVar (name, _, expr) = do
  value <- evalExpr expr
  ref <- lookupRef name
  writeRef ref value

evalExpr :: (IOE :> es, Error EvalError :> es, State Env :> es, State Uniq :> es, Reader ModuleName :> es) => Expr Name -> Eff es Value
evalExpr (Atom atom) = evalAtom atom
evalExpr (Call closure args) = do
  closure' <- evalAtom closure
  args' <- traverse evalAtom args
  case closure' of
    VFun env parameters body ->
      local do
        current <- get
        put $ fromMaybe current env -- If the environment is not captured, use the current environment.
        assign' (zip parameters args')
        evalExpr body
    _ -> throwError (NotClosure closure')
evalExpr (CallDirect name args) = do
  closure <- lookup name
  args' <- traverse evalAtom args
  case closure of
    VFun Nothing parameters body ->
      local do
        assign' (zip parameters args')
        evalExpr body
    _ -> throwError (NotFunction closure)
evalExpr (RawCall name _ args) = do
  args' <- traverse evalAtom args
  evalPrimitive name args'
evalExpr (Cast _ atom) = evalAtom atom
evalExpr (Let bindings body) = local do
  -- Generate references for each variable.
  refs <- traverse (\LocalDef {_variable} -> (_variable,) <$> newRef _variable) bindings
  assign refs
  -- Evaluate each expression and assign the result to the corresponding reference.
  for_ (zip (map snd refs) bindings) \(ref, LocalDef {_object}) ->
    evalObj ref.name _object >>= writeRef ref
  evalExpr body
evalExpr (Match scrutinee branches) = do
  scrutinee' <- evalExpr scrutinee
  evalMatch scrutinee' branches
evalExpr (Switch scrutinee branches defaultBranch) = do
  scrutinee' <- evalAtom scrutinee
  evalSwitch scrutinee' branches defaultBranch
evalExpr (SwitchUnboxed scrutinee branches defaultBranch) = do
  scrutinee' <- evalAtom scrutinee
  evalSwitchUnboxed scrutinee' branches defaultBranch
evalExpr (Destruct atom _ parameters body) = do
  value <- evalAtom atom
  case value of
    VPack _ refs -> do
      values <- traverse readRef refs
      local do
        assign' (zip parameters values)
        evalExpr body
    _ -> throwError (NotPack value)
evalExpr (DestructRecord atom fields body) = do
  value <- evalAtom atom
  case value of
    VRecord fields' -> do
      values <- traverse readRef fields'
      local do
        assign' (zip (Map.elems fields) (Map.elems values))
        evalExpr body
    _ -> throwError (NotPack value)
evalExpr (Assign name value body) = do
  value' <- evalExpr value
  assign' [(name, value')]
  evalExpr body
evalExpr (Error typ) = throwError (UnexpectedError typ)

evalSwitchUnboxed :: (Error EvalError :> es, State Env :> es, State Uniq :> es, Reader ModuleName :> es, IOE :> es) => Value -> [(Unboxed, Expr Name)] -> Expr Name -> Eff es Value
evalSwitchUnboxed _ [] defaultBranch = evalExpr defaultBranch
evalSwitchUnboxed (VUnboxed unboxed) ((unboxed', body) : rest) defaultBranch
  | unboxed == unboxed' = evalExpr body
  | otherwise = evalSwitchUnboxed (VUnboxed unboxed) rest defaultBranch
evalSwitchUnboxed _ _ _ = throwError NoMatch

evalSwitch :: (Error EvalError :> es, State Env :> es, State Uniq :> es, Reader ModuleName :> es, IOE :> es) => Value -> [(Tag, Expr Name)] -> Expr Name -> Eff es Value
evalSwitch _ [] defaultBranch = evalExpr defaultBranch
evalSwitch (VPack tag _) ((tag', body) : rest) defaultBranch
  | tag == tag' = evalExpr body
  | otherwise = evalSwitch (VPack tag []) rest defaultBranch
evalSwitch _ _ _ = throwError NoMatch

evalMatch :: (IOE :> es, Error EvalError :> es, State Env :> es, State Uniq :> es, Reader ModuleName :> es) => Value -> [Case Name] -> Eff es Value
evalMatch _ [] = throwError NoMatch
evalMatch scrutinee (Unpack (Con tag _) parameters body : rest) =
  case scrutinee of
    VPack tag' refs | tag == tag' -> do
      values <- traverse readRef refs
      local do
        assign' (zip parameters values)
        evalExpr body
    _ -> evalMatch scrutinee rest
evalMatch scrutinee (OpenRecord fields body : rest) =
  case scrutinee of
    VRecord fields' | Map.keysSet fields == Map.keysSet fields' -> do
      values <- traverse readRef fields'
      local do
        let bindings = zip (Map.elems fields) (Map.elems values)
        assign' bindings
        evalExpr body
    _ -> evalMatch scrutinee rest
evalMatch scrutinee (Exact unboxed body : rest) =
  case scrutinee of
    VUnboxed unboxed' | unboxed == unboxed' -> evalExpr body
    _ -> evalMatch scrutinee rest
evalMatch scrutinee (Bind name _ body : _) =
  local do
    assign' [(name, scrutinee)]
    evalExpr body

evalObj :: (State Uniq :> es, Reader ModuleName :> es, IOE :> es, Error EvalError :> es, State Env :> es) => Name -> Obj Name -> Eff es Value
evalObj _hint (Fun parameters body) = do
  env <- get @Env
  pure $ VFun (Just env) parameters body
evalObj _hint (Pack _ (Con tag types) args) = do
  args' <- traverse evalAtom args
  refs <- for (zip args' types) \(value, typ) -> do
    id <- newTemporalId "pack"
    let name = withMeta typ id
    ref <- newRef name
    writeRef ref value
    pure ref
  pure $ VPack tag refs
evalObj _hint (Record fields) =
  VRecord <$> for fields \field -> do
    id <- newTemporalId "record"
    let name = withMeta (typeOf field) id
    ref <- newRef name
    value <- evalAtom field
    writeRef ref value
    pure ref

evalPrimitive :: (Error EvalError :> es, IOE :> es) => Text -> [Value] -> Eff es Value
evalPrimitive name args = do
  case Map.lookup name primitives of
    Just prim -> prim args
    Nothing -> throwError (UndefinedPrimitive name)

local :: (State Env :> es) => Eff es a -> Eff es a
local action = do
  old <- get @Env
  result <- action
  put old
  pure result

evalAtom :: (IOE :> es, Error EvalError :> es, State Env :> es) => Atom Name -> Eff es Value
evalAtom = \case
  Var name -> lookup name
  Unboxed lit -> pure (VUnboxed lit)

type Name = Meta Type

type Env = Map Name Ref

lookupRef :: (Error EvalError :> es, State Env :> es, HasCallStack) => Name -> Eff es Ref
lookupRef name = do
  env <- get
  case Map.lookup name env of
    Just ref -> pure ref
    Nothing -> throwError (UnboundVariable name)

lookup :: (Error EvalError :> es, State Env :> es, IOE :> es, HasCallStack) => Name -> Eff es Value
lookup name = do
  env <- get
  case Map.lookup name env of
    Just ref -> readRef ref
    Nothing -> throwError (UnboundVariable name)

assign :: (State Env :> es) => [(Name, Ref)] -> Eff es ()
assign xs = modify (Map.fromList xs <>)

assign' :: (State Env :> es, IOE :> es) => [(Name, Value)] -> Eff es ()
assign' xs = do
  env <- traverse (\(name, value) -> newRef name >>= \ref -> writeRef ref value >> pure (name, ref)) xs
  assign env

data EvalError
  = UnboundVariable Name
  | InvalidTopVar Name
  | UnInitializedVariable Name
  | NotClosure Value
  | NotFunction Value
  | NotPack Value
  | UndefinedPrimitive Text
  | NoMatch
  | UnexpectedError Type
  | InvalidArguments Text [Value]
  | NoMain
  deriving stock (Show)

-- | A reference to a mutable value.
-- The name is uset for error messages. It describes the variable that the reference is bound to.
data Ref = Ref {name :: Name, ref :: IORef (Maybe Value)}

instance Show Ref where
  show (Ref name _) = show name

newRef :: (IOE :> es) => Name -> Eff es Ref
newRef name = Ref name <$> newIORef Nothing

readRef :: (IOE :> es, Error EvalError :> es) => Ref -> Eff es Value
readRef (Ref name ref) = do
  value <- readIORef ref
  case value of
    Just value -> pure value
    Nothing -> throwError (UnInitializedVariable name)

writeRef :: (IOE :> es) => Ref -> Value -> Eff es ()
writeRef (Ref _ ref) value = writeIORef ref (Just value)

data Value
  = VUnboxed Unboxed
  | VFun (Maybe Env) [Name] (Expr Name)
  | VPack Tag [Ref]
  | VRecord (Map Text Ref)
  deriving stock (Show)

primitives :: (Error EvalError :> es, IOE :> es) => Map Text ([Value] -> Eff es Value)
primitives =
  Map.fromList
    [ ( "malgo_unsafe_cast",
        \case
          [value] -> pure value
          values -> throwError $ InvalidArguments "malgo_unsafe_cast" values
      ),
      ( "malgo_add_int32_t",
        \case
          [VUnboxed (Int32 x), VUnboxed (Int32 y)] -> do
            pure $ VUnboxed (Int32 (x + y))
          values -> throwError $ InvalidArguments "malgo_add_int32_t" values
      ),
      ( "malgo_sub_int32_t",
        \case
          [VUnboxed (Int32 x), VUnboxed (Int32 y)] -> do
            pure $ VUnboxed (Int32 (x - y))
          values -> throwError $ InvalidArguments "malgo_sub_int32_t" values
      ),
      ( "malgo_int32_t_to_string",
        \case
          [VUnboxed (Int32 x)] -> pure $ VUnboxed $ String $ convertString $ show x
          values -> throwError $ InvalidArguments "malgo_int32_t_to_string" values
      ),
      ( "malgo_print_string",
        \case
          [VUnboxed (String x)] -> putText x >> pure (VPack Tuple [])
          values -> throwError $ InvalidArguments "malgo_print_string" values
      ),
      ( "malgo_newline",
        \case
          [VPack Tuple []] -> putText "\n" >> pure (VPack Tuple [])
          values -> throwError $ InvalidArguments "malgo_newline" values
      )
    ]