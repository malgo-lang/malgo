{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Malgo.Eval (EvalError, Env (..), newEnv, eval) where

import Control.Lens (makeFieldsId, over, view, _1)
import Data.Map qualified as Map
import Effectful.Error.Static (Error, throwError)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask, local, runReader)
import Log (logInfo_)
import Malgo.Core
import Malgo.Lens
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

makeFieldsId ''Env

data EvalError
  = UnboundVariable {location :: Location, name :: Name}
  | InvalidCut {location :: Location, value :: Value, covalue :: Covalue}
  | InvalidPositionDo {location :: Location, producer :: Producer}
  | NoExistField {location :: Location, givens :: [Text], want :: Text}
  | NotComatch {location :: Location, value :: Value}
  | NotConstruct {location :: Location, value :: Value}
  | InvalidPrim
      { location :: Location,
        tag :: Text,
        producers :: [Value],
        consumers :: [Covalue]
      }
  deriving (Show)

makeFieldsId ''EvalError

type EvalOf es = (Reader Env :> es, Error EvalError :> es)

getEnv :: (Reader Env :> es) => Eff es Env
getEnv = ask

withEnv :: (Reader Env :> es) => Env -> Eff es a -> Eff es a
withEnv env action = local (const env) action

lookup :: (Error EvalError :> es, Reader Env :> es) => Location -> Name -> Eff es Value
lookup loc name = do
  env <- getEnv
  case Map.lookup name env.variables of
    Just value -> pure value
    Nothing -> throwError $ UnboundVariable loc name

colookup :: (Error EvalError :> es, Reader Env :> es) => Location -> Name -> Eff es Covalue
colookup loc name = do
  env <- getEnv
  case Map.lookup name env.covariables of
    Just covalue -> pure covalue
    Nothing -> throwError $ UnboundVariable loc name

defLookup :: (Error EvalError :> es, Reader Env :> es) => Location -> Name -> Eff es Definition
defLookup loc name = do
  env <- getEnv
  case Map.lookup name env.toplevel of
    Just def -> pure def
    Nothing -> throwError $ UnboundVariable loc name

withVariables :: (Reader Env :> es) => Map Name Value -> Eff es a -> Eff es a
withVariables vars action = local @Env (over variables (vars <>)) action

withCovariables :: (Reader Env :> es) => Map Name Covalue -> Eff es a -> Eff es a
withCovariables covars action = local @Env (over covariables (covars <>)) action

runEval :: Env -> Eff (Reader Env : es) a -> Eff es a
runEval env = runReader env

eval :: (Log :> es, Error EvalError :> es) => Env -> Statement -> Eff es Value
eval env = runEval env . evalStatement

evalStatement :: (Reader Env :> es, Error EvalError :> es, Log :> es) => Statement -> Eff es Value
evalStatement (Prim {..}) | tag == "mul" = do
  producers' <- traverse evalProducer producers
  consumers' <- traverse evalConsumer consumers
  case (producers', consumers') of
    ([VInt x, VInt y], [consumer]) -> do
      let result = VInt (x * y)
      evalCut location result consumer
    _ -> throwError $ InvalidPrim {location, tag, producers = producers', consumers = consumers'}
evalStatement (Prim loc name args conts) = do
  args' <- traverse evalProducer args
  conts' <- traverse evalConsumer conts
  logInfo_ $ pShow ("Prim" :: Text, loc, name, args', conts')
  pure $ VInt 0
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
    $ evalStatement def.statement

evalCut :: (Log :> es, EvalOf es) => Location -> Value -> Covalue -> Eff es Value
evalCut _ value CFinish = pure value
evalCut _ value (CThen env name body) =
  withEnv env
    $ withVariables (Map.singleton name value)
    $ evalStatement body
evalCut loc (VComatch env clauses) (CDestruct name values covalues) = do
  go clauses
  where
    go ((Copattern {..}, body) : rest)
      | tag == name =
          withEnv env
            $ withVariables (Map.fromList (zip params values))
            $ withCovariables (Map.fromList (zip returns covalues))
            $ evalStatement body
      | otherwise = go rest
    go _ = throwError $ NoExistField loc (map (view $ _1 . tag) clauses) name
evalCut loc value CDestruct {} = throwError $ NotComatch loc value
evalCut loc (VConstruct name values covalues) (CMatch env clauses) = do
  go clauses
  where
    go ((Pattern {..}, body) : rest)
      | tag == name =
          withEnv env
            $ withVariables (Map.fromList (zip params values))
            $ withCovariables (Map.fromList (zip returns covalues))
            $ evalStatement body
      | otherwise = go rest
    go _ = throwError $ NoExistField loc (map (view $ _1 . tag) clauses) name
evalCut loc value CMatch {} = throwError $ NotConstruct loc value

evalProducer :: (Log :> es, EvalOf es) => Producer -> Eff es Value
evalProducer (Var loc name) = lookup loc name
evalProducer (Literal loc lit) = evalLiteral loc lit
evalProducer p@(Do loc _ _) = throwError (InvalidPositionDo loc p)
evalProducer (Construct _ name args conts) = do
  args' <- traverse evalProducer args
  conts' <- traverse evalConsumer conts
  pure $ VConstruct name args' conts'
evalProducer (Comatch _ branches) = VComatch <$> getEnv <*> pure branches

evalConsumer :: (Log :> es, EvalOf es) => Consumer -> Eff es Covalue
evalConsumer (Finish _) = pure CFinish
evalConsumer (Covar loc name) = colookup loc name
evalConsumer (Then _ name body) = CThen <$> getEnv <*> pure name <*> pure body
evalConsumer (Destruct _ name args conts) = do
  args' <- traverse evalProducer args
  conts' <- traverse evalConsumer conts
  pure $ CDestruct name args' conts'
evalConsumer (Match _ branches) = CMatch <$> getEnv <*> pure branches

evalLiteral :: Location -> Literal -> Eff es Value
evalLiteral _ (Int n) = pure $ VInt n