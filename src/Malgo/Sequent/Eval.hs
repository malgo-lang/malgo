{-# LANGUAGE UndecidableInstances #-}

module Malgo.Sequent.Eval (Value (..), EvalError (..), Env (..), emptyEnv) where

import Data.Map qualified as Map
import Data.Traversable (for)
import Debug.Trace (traceShowM)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Malgo.Prelude hiding (throwError)
import Malgo.Sequent.Core
import Malgo.Sequent.Fun (Literal, Name, Pattern (..), Tag (..))

data Value where
  Immediate :: Literal -> Value
  Struct :: Tag -> [Value] -> Value
  Function :: Env -> [Name] -> Statement Flat -> Value
  Record :: Env -> Map Text (Name, Statement Flat) -> Value
  Consumer :: Env -> Consumer Flat -> Value

deriving stock instance Show Value

data EvalError
  = UndefinedVariable Range Name
  | ExpectConsumer Range Value
  | ExpectFunction Range Value
  | ExpectRecord Range Value
  | NoSuchField Range Text Value
  | NoMatch Range Value
  deriving stock (Show)

type Toplevels = Map Name (Name, Statement Flat)

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

lookupToplevel :: (Reader Toplevels :> es, Error EvalError :> es) => Range -> Name -> Eff es (Name, Statement Flat)
lookupToplevel range name = do
  toplevels <- ask @Toplevels
  case Map.lookup name toplevels of
    Just value -> pure value
    Nothing -> throwError (UndefinedVariable range name)

class Eval t v where
  eval :: t -> v

instance (Error EvalError :> es, Reader Env :> es, Reader Toplevels :> es) => Eval (Statement Flat) (Eff es Value) where
  eval (Cut producer consumer) = do
    producer <- eval producer
    eval consumer (producer :: Value)
  eval (Join _ label consumer statement) = do
    env <- ask @Env
    let value = Consumer env consumer
    local (extendEnv label value) do
      eval statement
  eval (Primitive _ name producers consumer) = do
    producers <- traverse eval producers
    consumer <- eval consumer
    traceShowM (name, producers :: [Value], consumer :: Value)
    pure $ Struct Tuple []
  eval (Invoke range name consumer) = do
    (return, statement) <- lookupToplevel range name
    consumer <- eval consumer
    local (extendEnv return consumer) do
      eval statement

instance (Error EvalError :> es, Reader Env :> es) => Eval (Producer Flat) (Eff es Value) where
  eval (Var range name) = lookupEnv range name
  eval (Literal _ literal) = pure $ Immediate literal
  eval (Construct _ tag producers consumers) = do
    producers <- traverse eval producers
    consumers <- traverse eval consumers
    pure $ Struct tag (producers <> consumers)
  eval (Lambda _ parameters statement) = do
    env <- ask @Env
    pure $ Function env parameters statement
  eval (Object _ fields) = do
    env <- ask @Env
    pure $ Record env fields

instance (Error EvalError :> es, Reader Env :> es, Reader Toplevels :> es) => Eval (Consumer Flat) (Value -> Eff es Value) where
  eval (Label range label) given = do
    covalue <- lookupEnv range label
    case covalue of
      Consumer env consumer -> local (const env) $ eval consumer given
      _ -> throwError $ ExpectConsumer range covalue
  eval (Apply range producers consumers) given = do
    producers <- traverse eval producers
    consumers <- traverse eval consumers
    case given of
      Function env parameters statement ->
        local (const $ extendEnv' (zip parameters $ producers <> consumers) env) do
          eval statement
      _ -> throwError $ ExpectFunction range given
  eval (Project range field consumer) given = do
    consumer <- eval consumer
    case given of
      Record env fields -> do
        (name, statement) <- case Map.lookup field fields of
          Just value -> pure value
          Nothing -> throwError $ NoSuchField range field given
        local (const $ extendEnv name consumer env) do
          eval statement
      _ -> throwError $ ExpectRecord range given
  eval (Then _ name statement) given = do
    local (extendEnv name given) do
      eval statement
  eval (Finish _) given = pure given
  eval (Select range branches) given = go branches
    where
      go [] = throwError $ NoMatch range given
      go (Branch {pattern, statement} : rest) = do
        bindings <- match pattern given
        case bindings of
          Just bindings -> do
            local (extendEnv' bindings) $ eval statement
          Nothing -> go rest

match :: (Error EvalError :> es, Reader Env :> es, Reader Toplevels :> es) => Pattern -> Value -> Eff es (Maybe [(Name, Value)])
match (PVar _ name) value = pure $ Just [(name, value)]
match (PLiteral _ literal) (Immediate literal') | literal == literal' = pure $ Just []
match (Destruct _ tag patterns) (Struct tag' values) | tag == tag' = do
  bindings <- zipWithM match patterns values
  pure $ foldr (liftA2 (<>)) (Just []) bindings
match (Expand range patterns) (Record _ fields) = do
  let pairs = Map.intersectionWith (,) patterns fields
  pairs <- for pairs \(pattern, (return, statement)) -> do
    env <- ask @Env
    -- If the evaluation of `statement` finishes normally, the last consumer will be `Label range return`.
    -- By setting `return` to `Finish`, `eval statement` will return the value of the last producer.
    local (extendEnv return (Consumer env (Finish range))) do
      value <- eval statement
      match pattern value
  pure $ foldr (liftA2 (<>)) (Just []) pairs
match _ _ = pure Nothing

instance (Reader Env :> es) => Eval (Consumer Flat) (Eff es Value) where
  eval consumer = do
    env <- ask @Env
    pure $ Consumer env consumer
