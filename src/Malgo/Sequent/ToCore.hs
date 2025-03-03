module Malgo.Sequent.ToCore (toCore) where

import Data.Traversable (for)
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Malgo.Id
import Malgo.Module (ModuleName)
import Malgo.MonadUniq
import Malgo.Prelude
import Malgo.Sequent.Core (Producer (Do), Rank (..), Statement (Cut))
import Malgo.Sequent.Core qualified as C
import Malgo.Sequent.Fun

toCore :: (State Uniq :> es, Reader ModuleName :> es) => Program -> Eff es (C.Program Full)
toCore (Program definitions) = C.Program <$> traverse convert definitions

class Convert a b where
  convert :: a -> b

instance (State Uniq :> es, Reader ModuleName :> es) => Convert (Range, Name, Expr) (Eff es (Range, Name, Name, C.Statement Full)) where
  convert (range, name, body) = do
    return <- newTemporalId "return"
    body' <- convert body (C.Label @Full range return)
    pure
      ( range,
        name,
        return,
        body'
      )

instance (State Uniq :> es, Reader ModuleName :> es) => Convert Expr (C.Consumer Full -> Eff es (C.Statement Full)) where
  convert (Let range name value body) consumer = do
    body <- convert body consumer
    convert value (C.Then @Full range name body)
  convert (Apply range f args) consumer = do
    args <- traverse convert args
    convert f (C.Apply @Full range args [consumer])
  convert (Project range expr field) consumer = do
    convert expr (C.Project @Full range field consumer)
  convert (Primitive range operator args) consumer = do
    args <- traverse convert args
    pure $ C.Primitive range operator args consumer
  convert (Select range scrutinee branches) consumer = do
    branches <- traverse (convert consumer) branches
    convert scrutinee (C.Select @Full range branches)
  convert (Invoke range name) consumer = pure $ C.Invoke range name consumer
  convert expr consumer = do
    expr' <- convert expr
    pure $ Cut expr' consumer

instance (State Uniq :> es, Reader ModuleName :> es) => Convert Expr (Eff es (C.Producer Full)) where
  convert (Var range name) = pure $ C.Var range name
  convert (Literal range literal) = pure $ C.Literal range literal
  convert (Construct range tag arguments) = C.Construct range tag <$> traverse convert arguments <*> pure []
  convert producer@(Let range _ _ _) = do
    return <- newTemporalId "return"
    C.Do range return <$> convert producer (C.Label @Full range return)
  convert (Lambda range params body) = do
    return <- newTemporalId "return"
    body' <- convert body
    pure $ C.Lambda range (params <> [return]) (C.Cut body' (C.Label range return))
  convert (Object range fields) = do
    fields <- for fields \body -> do
      body <- convert body
      return <- newTemporalId "return"
      pure (return, Cut body (C.Label range return))
    pure $ C.Object range fields
  convert producer@(Apply range _ _) = do
    return <- newTemporalId "return"
    Do range return <$> convert producer (C.Label @Full range return)
  convert producer@(Project range _ _) = do
    return <- newTemporalId "return"
    Do range return <$> convert producer (C.Label @Full range return)
  convert producer@(Primitive range _ _) = do
    return <- newTemporalId "return"
    Do range return <$> convert producer (C.Label @Full range return)
  convert producer@(Select range _ _) = do
    return <- newTemporalId "return"
    Do range return <$> convert producer (C.Label @Full range return)
  convert producer@(Invoke range _) = do
    return <- newTemporalId "return"
    Do range return <$> convert producer (C.Label @Full range return)

instance (State Uniq :> es, Reader ModuleName :> es) => Convert (C.Consumer Full) (Branch -> Eff es (C.Branch Full)) where
  convert consumer (Branch range pattern body) = do
    body' <- convert body consumer
    pure $ C.Branch range pattern body'
