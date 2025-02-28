module Malgo.Sequent.Eval (Value (..), EvalError (..), Env (..), emptyEnv) where

import Data.Map qualified as Map
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Malgo.Prelude hiding (throwError)
import Malgo.Sequent.Core
import Malgo.Sequent.Fun (Literal, Name, Tag)

data Value where
  Literal :: Literal -> Value
  Struct :: Tag -> [Value] -> Value
  Function :: Env -> [Name] -> Statement Zero -> Value
  Object :: Env -> Map Text (Name, Statement Zero) -> Value

deriving stock instance Show Value

data EvalError = UndefinedVariable Range Name
  deriving stock (Show)

data Env = Env
  { parent :: Maybe Env,
    bindings :: Map Name Value
  }
  deriving stock (Show)

emptyEnv :: Env
emptyEnv = Env Nothing mempty

extendEnv :: Name -> Value -> Env -> Env
extendEnv name value env = env {bindings = Map.insert name value env.bindings}

extendEnv' :: [(Name, Value)] -> Env -> Env
extendEnv' bindings env = env {bindings = foldr (uncurry Map.insert) env.bindings bindings}

lookupEnv :: (Error EvalError :> es, Reader Env :> es) => Range -> Name -> Eff es Value
lookupEnv range name = do
  env <- ask @Env
  case Map.lookup name env.bindings of
    Just value -> pure value
    Nothing -> throwError (UndefinedVariable range name)

class Eval t v where
  eval :: t -> v

instance (Error EvalError :> es, Reader Env :> es) => Eval (Statement Zero) (Eff es Value) where
  eval (Cut producer consumer) = do
    producer <- eval producer
    eval consumer (producer :: Value)

instance (Error EvalError :> es, Reader Env :> es) => Eval (Producer Zero) (Eff es Value)

instance (Error EvalError :> es, Reader Env :> es) => Eval (Consumer Zero) (Value -> Eff es Value)
