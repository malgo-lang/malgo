{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Malgo.Core.Eval (eval) where

import Data.Map qualified as Map
import Effectful
import Effectful.Error.Static (Error, catchError, throwError)
import Effectful.State.Static.Local
import Malgo.Core.Syntax
import Malgo.Core.Type
import Malgo.Id
import Malgo.Prelude hiding (catchError, lookup, throwError)

eval :: (IOE :> es, Error EvalError :> es) => Program Name -> Eff es ()
eval program = evalState @Env mempty do
  traverse_ evalTopFun program.topFuns
  traverse_ initTopVar program.topVars
  traverse_ evalTopVar program.topVars
  undefined

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

evalTopVar :: (State Env :> es, Error EvalError :> es, IOE :> es) => (Name, b, Expr Name) -> Eff es ()
evalTopVar (name, _, expr) = do
  -- TODO: Better error message.
  -- If UnInitializedVariable is thrown, it means that some variables in expr is read before they are initialized.
  -- Core doen't allow this, so we can assume that the error is caused by a bug in the compiler.
  value <-
    evalExpr expr `catchError` \_ -> \case
      UnInitializedVariable _ -> throwError (InvalidTopVar name)
      err -> throwError err
  ref <- lookupRef name
  writeRef ref value

evalExpr :: (IOE :> es, Error EvalError :> es, State Env :> es) => Expr Name -> Eff es Value
evalExpr (Atom atom) = evalAtom atom

evalAtom :: (IOE :> es, Error EvalError :> es, State Env :> es) => Atom Name -> Eff es Value
evalAtom = \case
  Var name -> lookup name
  Unboxed lit -> pure (VUnboxed lit)

type Name = Meta Id

type Env = Map Name Ref

lookupRef :: (Error EvalError :> es, State Env :> es) => Name -> Eff es Ref
lookupRef name = do
  env <- get
  case Map.lookup name env of
    Just ref -> pure ref
    Nothing -> throwError (UnboundVariable name)

lookup :: (Error EvalError :> es, State Env :> es, IOE :> es) => Name -> Eff es Value
lookup name = do
  env <- get
  case Map.lookup name env of
    Just ref -> readRef ref
    Nothing -> throwError (UnboundVariable name)

assign :: (State Env :> es) => [(Name, Ref)] -> Eff es ()
assign xs = modify (Map.fromList xs <>)

data EvalError
  = UnboundVariable Name
  | InvalidTopVar Name
  | UnInitializedVariable Name
  deriving stock (Show)

-- | A reference to a mutable value.
-- The name is uset for error messages. It describes the variable that the reference is bound to.
data Ref = Ref Name (IORef (Maybe Value))

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
  | VPack Con [Ref]
  | VRecord (Map Text Ref)