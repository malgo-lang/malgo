module Malgo.Sequent.ToCore (toCore, ToCorePass (..)) where

import Data.Traversable (for)
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Malgo.Id
import Malgo.Module (ModuleName)
import Malgo.MonadUniq
import Malgo.Pass
import Malgo.Prelude
import Malgo.Sequent.Core (Producer (Do), Rank (..), Statement (Cut))
import Malgo.Sequent.Core qualified as C
import Malgo.Sequent.Fun

data ToCorePass = ToCorePass

instance Pass ToCorePass where
  type Input ToCorePass = Program
  type Output ToCorePass = C.Program Full
  type ErrorType ToCorePass = Void
  type Effects ToCorePass es = (State Uniq :> es, Reader ModuleName :> es)

  runPassImpl _ = toCore

toCore :: (State Uniq :> es, Reader ModuleName :> es) => Program -> Eff es (C.Program Full)
toCore (Program {..}) = do
  definitions <- traverse convertDefinition definitions
  pure C.Program {definitions, dependencies}

convertDefinition :: (State Uniq :> es, Reader ModuleName :> es) => (Range, Name, Expr) -> Eff es (Range, Name, Name, C.Statement Full)
convertDefinition (range, name, body) = do
  return <- newTemporalId "return"
  body' <- toStatement body (C.Label @Full range return)
  pure
    ( range,
      name,
      return,
      body'
    )

toStatement :: (State Uniq :> es, Reader ModuleName :> es) => Expr -> (C.Consumer Full -> Eff es (C.Statement Full))
toStatement (Let range name value body) consumer = do
  body <- toStatement body consumer
  toStatement value (C.Then @Full range name body)
toStatement (Apply range f args) consumer = do
  args <- traverse toProducer args
  toStatement f (C.Apply @Full range args [consumer])
toStatement (Project range expr field) consumer = do
  toStatement expr (C.Project @Full range field consumer)
toStatement (Primitive range operator args) consumer = do
  args <- traverse toProducer args
  pure $ C.Primitive range operator args consumer
toStatement (Select range scrutinee branches) consumer = do
  branches <- traverse (convertBranch consumer) branches
  toStatement scrutinee (C.Select @Full range branches)
toStatement (Invoke range name) consumer = pure $ C.Invoke range name consumer
toStatement expr consumer = do
  expr' <- toProducer expr
  pure $ Cut expr' consumer

toProducer :: (State Uniq :> es, Reader ModuleName :> es) => Expr -> Eff es (C.Producer Full)
toProducer (Var range name) = pure $ C.Var range name
toProducer (Literal range literal) = pure $ C.Literal range literal
toProducer (Construct range tag arguments) = C.Construct range tag <$> traverse toProducer arguments <*> pure []
toProducer producer@(Let range _ _ _) = do
  return <- newTemporalId "return"
  C.Do range return <$> toStatement producer (C.Label @Full range return)
toProducer (Lambda range params body) = do
  return <- newTemporalId "return"
  body' <- toStatement body (C.Label @Full range return)
  pure $ C.Lambda range (params <> [return]) body'
toProducer (Object range fields) = do
  fields <- for fields \body -> do
    return <- newTemporalId "return"
    body <- toStatement body (C.Label @Full range return)
    pure (return, body)
  pure $ C.Object range fields
toProducer producer@(Apply range _ _) = do
  return <- newTemporalId "return"
  Do range return <$> toStatement producer (C.Label @Full range return)
toProducer producer@(Project range _ _) = do
  return <- newTemporalId "return"
  Do range return <$> toStatement producer (C.Label @Full range return)
toProducer producer@(Primitive range _ _) = do
  return <- newTemporalId "return"
  Do range return <$> toStatement producer (C.Label @Full range return)
toProducer producer@(Select range _ _) = do
  return <- newTemporalId "return"
  Do range return <$> toStatement producer (C.Label @Full range return)
toProducer producer@(Invoke range _) = do
  return <- newTemporalId "return"
  Do range return <$> toStatement producer (C.Label @Full range return)

convertBranch :: (State Uniq :> es, Reader ModuleName :> es) => C.Consumer Full -> Branch -> Eff es (C.Branch Full)
convertBranch consumer (Branch range pattern body) = do
  body' <- toStatement body consumer
  pure $ C.Branch range pattern body'
