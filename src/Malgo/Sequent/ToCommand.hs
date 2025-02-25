module Malgo.Sequent.ToCommand (toCommand) where

import Control.Lens (traverseOf, _2)
import Effectful
import Effectful.State.Static.Local (State)
import Malgo.MonadUniq
import Malgo.Prelude
import Malgo.Sequent.Command (Code)
import Malgo.Sequent.Command qualified as C
import Malgo.Sequent.Core
import Malgo.Sequent.Fun (Name)

toCommand :: (State Uniq :> es) => Program Zero -> Eff es C.Program
toCommand (Program definitions) = do
  program <- C.Program <$> traverse convert definitions
  C.lintProgram program >>= \case
    Right () -> pure program
    Left (cs, err) -> error $ show (cs, err)

class Convert a b where
  convert :: a -> b

instance (State Uniq :> es) => Convert (Range, Name, Name, Statement Zero) (Eff es (Range, Name, Name, Code)) where
  convert (range, name, return, body) = do
    body <- convert body
    pure (range, name, return, body)

instance (State Uniq :> es) => Convert (Statement Zero) (Eff es Code) where
  convert (Cut producer consumer) = do
    producer <- convert producer
    consumer <- convert consumer
    pure $ producer <> consumer
  convert (CutDo range name statement consumer) = do
    body <- convert statement
    let producer = [C.Do range name body]
    consumer <- convert consumer
    pure $ producer <> consumer
  convert (Primitive range operator producers consumers) = do
    producers <- foldMap convert producers
    consumers <- map C.Suspend <$> traverse convert consumers
    pure $ producers <> consumers <> [C.Primitive range operator]
  convert (Invoke range name consumer) = do
    consumer <- C.Suspend <$> convert consumer
    pure $ consumer : [C.Invoke range name]

instance (State Uniq :> es) => Convert (Producer Zero) (Eff es Code) where
  convert (Var range name) = pure [C.Fetch range name]
  convert (Literal range literal) = pure [C.Push range literal]
  convert (Construct range tag producers consumers) = do
    producers <- foldMap convert producers
    consumers <- map C.Suspend <$> traverse convert consumers
    pure $ producers <> consumers <> [C.Construct range tag (length producers + length consumers)]
  convert (Lambda range parameters body) = do
    body <- convert body
    pure [C.Lambda range parameters body]
  convert (Object range fields) = do
    fields <- traverseOf (traverse . _2) convert fields
    pure $ [C.Object range fields]

instance (State Uniq :> es) => Convert (Consumer Zero) (Eff es Code) where
  convert (Label range name) = pure [C.Resume range name]
  convert (Apply range producers consumers) = do
    producers <- foldMap convert producers
    consumers <- map C.Suspend <$> traverse convert consumers
    pure $ producers <> consumers <> [C.Apply range (length producers + length consumers)]
  convert (Project range field consumer) = do
    consumer <- convert consumer
    pure $ [C.Proj range field] <> consumer
  convert (Then range name consumer) = do
    consumer <- convert consumer
    pure [C.Then range name consumer]
  convert (Finish range) = pure [C.Finish range]
  convert (Select range branches) = do
    branches <- traverse convert branches
    pure [C.Select range branches]

instance (State Uniq :> es) => Convert (Branch Zero) (Eff es C.Branch) where
  convert (Branch range pattern consumer) = do
    consumer <- convert consumer
    pure $ C.Branch range pattern consumer
