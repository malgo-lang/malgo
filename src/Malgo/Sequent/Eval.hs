{-# LANGUAGE UndecidableInstances #-}

module Malgo.Sequent.Eval (Value (..), EvalError (..), Env (..), emptyEnv, evalProgram) where

import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Traversable (for)
import Debug.Trace (traceShowM)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Malgo.Id
import Malgo.Prelude hiding (throwError)
import Malgo.Sequent.Core
import Malgo.Sequent.Fun (HasRange (..), Literal, Name, Pattern (..), Tag (..))

data Value where
  Immediate :: Literal -> Value
  Struct :: Tag -> [Value] -> Value
  Function :: Env -> [Name] -> Statement Join -> Value
  Record :: Env -> Map Text (Name, Statement Join) -> Value
  Consumer :: Env -> Consumer Join -> Value

deriving stock instance Show Value

data EvalError
  = UndefinedVariable Range Name
  | ExpectConsumer Range Value
  | ExpectFunction Range Value
  | ExpectRecord Range Value
  | NoSuchField Range Text Value
  | NoMatch Range Value
  deriving stock (Show)

type Toplevels = Map Name (Name, Statement Join)

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

jump :: (Error EvalError :> es, Reader Toplevels :> es, Reader Env :> es) => Range -> Name -> Value -> Eff es Value
jump range name value = do
  covalue <- lookupEnv range name
  case covalue of
    Consumer env consumer -> local (const env) $ evalConsumer consumer value
    _ -> throwError $ ExpectConsumer range value

lookupToplevel :: (Reader Toplevels :> es, Error EvalError :> es) => Range -> Name -> Eff es (Name, Statement Join)
lookupToplevel range name = do
  toplevels <- ask @Toplevels
  case Map.lookup name toplevels of
    Just value -> pure value
    Nothing -> throwError (UndefinedVariable range name)

evalProgram :: (Error EvalError :> es) => Program Join -> Eff es Value
evalProgram (Program definitions) = do
  let toplevels = Map.fromList [(name, (return, statement)) | (_, name, return, statement) <- definitions]
  let (return, statement) =
        Map.keys toplevels & find (\name -> name.name == "main") & \case
          Just name -> fromJust $ Map.lookup name toplevels -- It is safe to use fromJust here because the main function is guaranteed to exist.
          Nothing -> error "main function not found" -- TODO: Error handling
  runReader toplevels
    $ runReader emptyEnv
    $ local (extendEnv return (Consumer emptyEnv (Finish (range statement))))
    $ evalStatement statement

evalStatement :: (Error EvalError :> es, Reader Env :> es, Reader Toplevels :> es) => Statement Join -> Eff es Value
evalStatement (Cut producer consumer) = do
  value <- evalProducer producer
  jump (range producer) consumer value
evalStatement (Join _ label consumer statement) = do
  env <- ask @Env
  let value = Consumer env consumer
  local (extendEnv label value) do
    evalStatement statement
evalStatement (Primitive range name producers consumer) = do
  producers <- traverse evalProducer producers
  covalue <- lookupEnv range consumer
  traceShowM (name, producers, covalue)
  pure $ Struct Tuple []
evalStatement (Invoke range name consumer) = do
  (return, statement) <- lookupToplevel range name
  covalue <- lookupEnv range consumer
  local (extendEnv return covalue) do
    evalStatement statement

evalProducer :: (Error EvalError :> es, Reader Env :> es) => Producer Join -> Eff es Value
evalProducer (Var range name) = lookupEnv range name
evalProducer (Literal _ literal) = pure $ Immediate literal
evalProducer (Construct range tag producers consumers) = do
  producers <- traverse evalProducer producers
  consumers <- traverse (lookupEnv range) consumers
  pure $ Struct tag (producers <> consumers)
evalProducer (Lambda _ parameters statement) = do
  env <- ask @Env
  pure $ Function env parameters statement
evalProducer (Object _ fields) = do
  env <- ask @Env
  pure $ Record env fields

evalConsumer :: (Error EvalError :> es, Reader Env :> es, Reader Toplevels :> es) => Consumer Join -> Value -> Eff es Value
evalConsumer (Label range label) given = do
  covalue <- lookupEnv range label
  case covalue of
    Consumer env consumer -> local (const env) $ evalConsumer consumer given
    _ -> throwError $ ExpectConsumer range covalue
evalConsumer (Apply range producers consumers) given = do
  producers <- traverse evalProducer producers
  consumers <- traverse (lookupEnv range) consumers
  case given of
    Function env parameters statement ->
      local (const $ extendEnv' (zip parameters $ producers <> consumers) env) do
        evalStatement statement
    _ -> throwError $ ExpectFunction range given
evalConsumer (Project range field consumer) given = do
  covalue <- lookupEnv range consumer
  case given of
    Record env fields -> do
      (name, statement) <- case Map.lookup field fields of
        Just value -> pure value
        Nothing -> throwError $ NoSuchField range field given
      local (const $ extendEnv name covalue env) do
        evalStatement statement
    _ -> throwError $ ExpectRecord range given
evalConsumer (Then _ name statement) given = do
  local (extendEnv name given) do
    evalStatement statement
evalConsumer (Finish _) given = pure given
evalConsumer (Select range branches) given = go branches
  where
    go [] = throwError $ NoMatch range given
    go (Branch {pattern, statement} : rest) = do
      bindings <- match pattern given
      case bindings of
        Just bindings -> do
          local (extendEnv' bindings) $ evalStatement statement
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
      value <- evalStatement statement
      match pattern value
  pure $ foldr (liftA2 (<>)) (Just []) pairs
match _ _ = pure Nothing
