module Malgo.Sequent.ToCore (toCore) where

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

toCore :: (State Uniq :> es, Reader ModuleName :> es) => Program -> Eff es (C.Program One)
toCore (Program definitions) = C.Program <$> traverse convert definitions

class Convert a b where
  convert :: a -> b

instance (State Uniq :> es, Reader ModuleName :> es) => Convert (Range, Name, Expr) (Eff es (Range, Name, Name, C.Statement One)) where
  convert (range, name, body) = do
    return <- newTemporalId "return"
    body' <- convert body
    pure
      ( range,
        name,
        return,
        C.Cut body' (C.Label range return)
      )

instance (State Uniq :> es, Reader ModuleName :> es) => Convert Expr (Eff es (C.Producer One)) where
  convert (Var range name) = pure $ C.Var range name
  convert (Literal range literal) = pure $ C.Literal range literal
  convert (Construct range tag arguments) = C.Construct range tag <$> traverse convert arguments <*> pure []
  convert (Let range name value body) = do
    value <- convert value
    body <- convert body
    ret <- newTemporalId "return"
    pure $ C.Do range ret $ C.Cut value (C.Then range name (C.Cut body (C.Label range ret)))
  convert (Lambda range params body) = do
    return <- newTemporalId "return"
    body' <- convert body
    pure $ C.Lambda range (params <> [return]) (C.Cut body' (C.Label range return))
  convert (Object range fields) = do
    return <- newTemporalId "return"
    fields' <- traverse convert fields
    let fields'' = fmap (\expr -> (Cut expr (C.Label range return))) fields'
    pure $ C.Object range fields''
  convert (Apply range f args) = do
    f' <- convert f
    args' <- traverse convert args
    return <- newTemporalId "return"
    pure $ Do range return $ Cut f' $ C.Apply range args' [C.Label range return]
  convert (Project range expr field) = do
    expr' <- convert expr
    return <- newTemporalId "return"
    pure $ Do range return $ Cut expr' $ C.Project range field (C.Label range return)
  convert (Primitive range operator args) = do
    args' <- traverse convert args
    return <- newTemporalId "return"
    pure $ Do range return $ C.Primitive range operator args' [C.Label range return]
  convert (Select range scrutinee branches) = do
    scrutinee' <- convert scrutinee
    return <- newTemporalId "return"
    branches' <- traverse (convert return) branches
    pure $ Do range return $ Cut scrutinee' $ C.Select range branches'
  convert (Invoke range name) = do
    return <- newTemporalId "return"
    pure $ Do range return $ C.Invoke range name (C.Label range return)

instance (State Uniq :> es, Reader ModuleName :> es) => Convert Name (Branch -> Eff es (C.Branch One)) where
  convert return (Branch range pattern body) = do
    body' <- convert body
    pure $ C.Branch range pattern (C.Cut body' (C.Label range return))
