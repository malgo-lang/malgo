module Malgo.Sequent.Core.Join (joinProgram) where

import Control.Lens
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local
import Malgo.Id
import Malgo.Module (ModuleName)
import Malgo.MonadUniq
import Malgo.Prelude
import Malgo.Sequent.Core
import Malgo.Sequent.Fun (Name)

joinProgram :: (State Uniq :> es, Reader ModuleName :> es) => Program Flat -> Eff es (Program Join)
joinProgram Program {..} = do
  definitions <- traverse joinDefinition definitions
  pure Program {definitions}

joinDefinition :: (State Uniq :> es, Reader ModuleName :> es) => (Range, Name, Name, Statement Flat) -> Eff es (Range, Name, Name, Statement Join)
joinDefinition (range, name, return, statement) = do
  statement <- runJoin $ joinStatement statement
  pure (range, name, return, statement)

runJoin :: Eff (Writer (Endo b) : es) b -> Eff es b
runJoin m = uncurry (flip appEndo) <$> runWriter m

joinStatement :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo (Statement Join)) :> es) => Statement Flat -> Eff es (Statement Join)
joinStatement (Cut producer consumer) = do
  producer <- joinProducer producer
  consumer <- joinConsumer consumer
  pure $ Cut producer consumer
joinStatement (CutDo range name statement consumer) = do
  statement <- joinStatement statement
  consumer <- joinConsumer' consumer
  pure $ Join range name consumer statement
joinStatement (Primitive range name producers return) = do
  producers <- traverse joinProducer producers
  return <- joinConsumer return
  pure $ Primitive range name producers return
joinStatement (Invoke range name return) = do
  return <- joinConsumer return
  pure $ Invoke range name return

joinProducer :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo (Statement Join)) :> es) => Producer Flat -> Eff es (Producer Join)
joinProducer (Var range name) = pure $ Var range name
joinProducer (Literal range literal) = pure $ Literal range literal
joinProducer (Construct range tag producers returns) = do
  producers <- traverse joinProducer producers
  returns <- traverse joinConsumer returns
  pure $ Construct range tag producers returns
joinProducer (Lambda range names statement) = do
  statement <- runJoin $ joinStatement statement
  pure $ Lambda range names statement
joinProducer (Object range fields) = do
  fields <- traverseOf (traverse . _2) (runJoin . joinStatement) fields
  pure $ Object range fields

joinConsumer :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo (Statement Join)) :> es) => Consumer Flat -> Eff es Name
joinConsumer (Label _ name) = pure name
joinConsumer (Apply range producers returns) = do
  producers <- traverse joinProducer producers
  returns <- traverse joinConsumer returns
  tellJoin range "apply" $ Apply range producers returns
joinConsumer (Project range field return) = do
  return <- joinConsumer return
  tellJoin range "project" $ Project range field return
joinConsumer (Then range name statement) = do
  statement <- runJoin $ joinStatement statement
  tellJoin range "then" $ Then range name statement
joinConsumer (Finish range) = tellJoin range "finish" $ Finish range
joinConsumer (Select range branches) = do
  branches <- traverse joinBranch branches
  tellJoin range "select" $ Select range branches

joinConsumer' :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo (Statement Join)) :> es) => Consumer Flat -> Eff es (Consumer Join)
joinConsumer' (Label range name) = pure (Label range name)
joinConsumer' (Apply range producers returns) = do
  producers <- traverse joinProducer producers
  returns <- traverse joinConsumer returns
  pure $ Apply range producers returns
joinConsumer' (Project range field return) = do
  return <- joinConsumer return
  pure $ Project range field return
joinConsumer' (Then range name statement) = do
  statement <- runJoin $ joinStatement statement
  pure $ Then range name statement
joinConsumer' (Finish range) = pure $ Finish range
joinConsumer' (Select range branches) = do
  branches <- traverse joinBranch branches
  pure $ Select range branches

joinBranch :: (State Uniq :> es, Reader ModuleName :> es) => Branch Flat -> Eff es (Branch Join)
joinBranch (Branch range pattern statement) = do
  statement <- runJoin $ joinStatement statement
  pure $ Branch range pattern statement

tellJoin :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo (Statement Join)) :> es) => Range -> Text -> Consumer Join -> Eff es Id
tellJoin range name consumer = do
  name <- newTemporalId name
  tell $ Endo $ Join range name consumer
  pure name