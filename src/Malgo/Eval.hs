{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Malgo.Eval (Eval, EvalError, Env, newEnv, eval) where

import Control.Lens (view, _1)
import Data.Map qualified as Map
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Log (Log)
import Effectful.Reader.Static (ask, local, runReader)
import Log (logInfo_)
import Malgo.Core
import Malgo.Location
import Malgo.Name
import Malgo.Prelude

data Env = Env
  { variables :: Map Name Value,
    covariables :: Map Name Covalue,
    toplevel :: Map Name Definition
  }
  deriving (Show, Eq)

newEnv :: [Definition] -> Env
newEnv defs = Env mempty mempty (Map.fromList [(def.name, def) | def <- defs])

instance Semigroup Env where
  Env vars1 covars1 defs1 <> Env vars2 covars2 defs2 =
    Env (vars1 <> vars2) (covars1 <> covars2) (defs1 <> defs2)

instance Monoid Env where
  mempty = Env mempty mempty mempty

data Value
  = VInt Int
  | VConstruct Text [Value] [Covalue]
  | VComatch Env [(Copattern, Statement)]
  deriving (Show, Eq)

data Covalue
  = CFinish
  | CThen Env Name Statement
  | CDestruct Text [Value] [Covalue]
  | CMatch Env [(Pattern, Statement)]
  deriving (Show, Eq)

data EvalError
  = UnboundVariable Location Name
  | InvalidCut Location Value Covalue
  | InvalidPositionDo Producer
  | NoExistField Location [Text] Text
  | NotComatch Location Value
  | NotConstruct Location Value
  deriving (Show)

instance HasLocation EvalError where
  location (UnboundVariable loc _) = loc
  location (InvalidCut loc _ _) = loc
  location (InvalidPositionDo p) = location p
  location (NoExistField loc _ _) = loc
  location (NotComatch loc _) = loc
  location (NotConstruct loc _) = loc

data Eval :: Effect where
  GetEnv :: Eval m Env
  WithEnv :: Env -> m a -> Eval m a
  WithVariables :: Map Name Value -> m a -> Eval m a
  WithCovariables :: Map Name Covalue -> m a -> Eval m a
  ThrowError :: EvalError -> Eval m a

type instance DispatchOf Eval = Dynamic

getEnv :: (HasCallStack, Eval :> es) => Eff es Env
getEnv = send GetEnv

withEnv :: (HasCallStack, Eval :> es) => Env -> Eff es a -> Eff es a
withEnv env action = send $ WithEnv env action

lookup :: (HasCallStack, Eval :> es) => Location -> Name -> Eff es Value
lookup loc name = do
  env <- getEnv
  case Map.lookup name env.variables of
    Just value -> pure value
    Nothing -> throwError $ UnboundVariable loc name

colookup :: (HasCallStack, Eval :> es) => Location -> Name -> Eff es Covalue
colookup loc name = do
  env <- getEnv
  case Map.lookup name env.covariables of
    Just covalue -> pure covalue
    Nothing -> throwError $ UnboundVariable loc name

defLookup :: (HasCallStack, Eval :> es) => Location -> Name -> Eff es Definition
defLookup loc name = do
  env <- getEnv
  case Map.lookup name env.toplevel of
    Just def -> pure def
    Nothing -> throwError $ UnboundVariable loc name

withVariables :: (HasCallStack, Eval :> es) => Map Name Value -> Eff es a -> Eff es a
withVariables vars action = send $ WithVariables vars action

withCovariables :: (HasCallStack, Eval :> es) => Map Name Covalue -> Eff es a -> Eff es a
withCovariables covars action = send $ WithCovariables covars action

throwError :: (HasCallStack, Eval :> es) => EvalError -> Eff es a
throwError err = send (ThrowError err)

runEval :: (Error EvalError :> es) => Env -> Eff (Eval : es) a -> Eff es a
runEval env0 = reinterpret (runReader env0) $ \localEnv operation ->
  case operation of
    GetEnv -> ask @Env
    WithEnv env action ->
      localSeqUnlift localEnv $ \unlift ->
        local (const env) (unlift action)
    WithVariables vars action ->
      localSeqUnlift localEnv $ \unlift ->
        local
          (\env -> env {variables = vars <> env.variables})
          (unlift action)
    WithCovariables covars action ->
      localSeqUnlift localEnv $ \unlift ->
        local
          (\env -> env {covariables = covars <> env.covariables})
          (unlift action)
    ThrowError err -> Error.throwError err

eval :: (Log :> es, Error EvalError :> es) => Env -> Statement -> Eff es ()
eval env = runEval env . evalStatement

evalStatement :: (Log :> es, Eval :> es) => Statement -> Eff es ()
evalStatement (Prim loc name args cont) = do
  args' <- traverse evalProducer args
  cont' <- evalConsumer cont
  logInfo_ $ pShow ("Prim" :: Text, loc, name, args', cont')
evalStatement (Switch loc scrutinee branches defaultBranch) = do
  scrutinee' <- evalProducer scrutinee
  go scrutinee' branches
  where
    go _ [] = evalStatement defaultBranch
    go scrutinee' ((lit, branch) : rest) = do
      lit' <- evalLiteral loc lit
      if scrutinee' == lit'
        then evalStatement branch
        else go scrutinee' rest
evalStatement (Cut _ (Do _ name body) cont) = do
  cont' <- evalConsumer cont
  withCovariables (Map.singleton name cont') $ evalStatement body
evalStatement (Cut loc producer consumer) = do
  producer' <- evalProducer producer
  consumer' <- evalConsumer consumer
  evalCut loc producer' consumer'
evalStatement (Invoke loc name args conts) = do
  args' <- traverse evalProducer args
  conts' <- traverse evalConsumer conts
  def <- defLookup loc name
  withVariables (Map.fromList (zip def.params args'))
    $ withCovariables (Map.fromList (zip def.returns conts'))
    $ evalStatement def.body

evalCut :: (Log :> es, Eval :> es) => Location -> Value -> Covalue -> Eff es ()
evalCut _ value CFinish = logInfo_ $ pShow (CFinish, value)
evalCut _ value (CThen env name body) =
  withEnv env
    $ withVariables (Map.singleton name value)
    $ evalStatement body
evalCut loc (VComatch env clauses) (CDestruct name args conts) = do
  go clauses
  where
    go (((field, params, rets), body) : rest)
      | field == name =
          withEnv env
            $ withVariables (Map.fromList (zip params args))
            $ withCovariables (Map.fromList (zip rets conts))
            $ evalStatement body
      | otherwise = go rest
    go _ = throwError $ NoExistField loc (map (view $ _1 . _1) clauses) name
evalCut loc value CDestruct {} = throwError $ NotComatch loc value
evalCut loc (VConstruct name args conts) (CMatch env clauses) = do
  go clauses
  where
    go (((field, params, rets), body) : rest)
      | field == name =
          withEnv env
            $ withVariables (Map.fromList (zip params args))
            $ withCovariables (Map.fromList (zip rets conts))
            $ evalStatement body
      | otherwise = go rest
    go _ = throwError $ NoExistField loc (map (view $ _1 . _1) clauses) name
evalCut loc value CMatch {} = throwError $ NotConstruct loc value

evalProducer :: (Log :> es, Eval :> es) => Producer -> Eff es Value
evalProducer (Var loc name) = lookup loc name
evalProducer (Literal loc lit) = evalLiteral loc lit
evalProducer p@(Do _ _ _) = throwError (InvalidPositionDo p)
evalProducer (Construct _ name args conts) = do
  args' <- traverse evalProducer args
  conts' <- traverse evalConsumer conts
  pure $ VConstruct name args' conts'
evalProducer (Comatch _ branches) = VComatch <$> getEnv <*> pure branches

evalConsumer :: (Eval :> es, Log :> es) => Consumer -> Eff es Covalue
evalConsumer (Finish _) = pure CFinish
evalConsumer (Label loc name) = colookup loc name
evalConsumer (Then _ name body) = CThen <$> getEnv <*> pure name <*> pure body
evalConsumer (Destruct _ name args conts) = do
  args' <- traverse evalProducer args
  conts' <- traverse evalConsumer conts
  pure $ CDestruct name args' conts'
evalConsumer (Match _ branches) = CMatch <$> getEnv <*> pure branches

evalLiteral :: Location -> Literal -> Eff es Value
evalLiteral _ (Int n) = pure $ VInt n