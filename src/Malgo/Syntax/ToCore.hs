{-# LANGUAGE TypeFamilies #-}

module Malgo.Syntax.ToCore (toCore, ToCoreError) where

import Data.Traversable (for)
import Effectful.Error.Static (Error, throwError)
import Malgo.Core qualified as Core
import Malgo.Location
import Malgo.Name
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Unique (UniqueGen)

toCore :: (UniqueGen :> es, Error ToCoreError :> es) => [Definition Desugared Name] -> Eff es [Core.Definition]
toCore definitions = do
  traverse convert definitions

class Convert a r where
  convert :: a -> r

data ToCoreError
  = InvalidConsumer {location :: Location, term :: Term Desugared Name}
  | InvalidPattern {location :: Location, pattern :: Pattern Desugared Name}
  deriving (Show)

instance (UniqueGen :> es, Error ToCoreError :> es) => Convert (Definition Desugared Name) (Eff es Core.Definition) where
  convert Definition {..} = do
    result <- newName "result"
    statement <- convert term result
    pure
      Core.Definition
        { name,
          params,
          returns = returns <> [result],
          statement
        }

instance (UniqueGen :> es, Error ToCoreError :> es) => Convert (Term Desugared Name) (Eff es Core.Producer) where
  convert Var {..} = pure $ Core.Var {..}
  convert Literal {..} = do
    let literal' = convert literal
    pure $ Core.Literal {location, literal = literal'}
  convert Construct {..} = do
    producers' <- traverse convert producers
    consumers' <- traverse convert consumers
    pure
      Core.Construct
        { location,
          tag,
          producers = producers',
          consumers = consumers'
        }
  convert Comatch {..} = do
    clauses <- for coclauses \Coclause {..} -> do
      (copattern', cont) <- convertCopattern copattern
      statement <- convert term cont
      pure (copattern', statement)
    pure $ Core.Comatch {location, clauses}
    where
      convertCopattern CDestruct {..} = do
        cont <- newName "contCopattern"
        pure
          ( Core.Copattern
              { tag,
                params,
                returns = returns <> [cont]
              },
            cont
          )
  convert term@Destruct {location} = do
    cont <- newName "contDestruct"
    statement <- convert term cont
    pure
      Core.Do
        { location,
          name = cont,
          statement
        }
  convert term@Match {location} = do
    cont <- newName "contMatch"
    statement <- convert term cont
    pure
      Core.Do
        { location,
          name = cont,
          statement
        }
  convert term@Prim {location} = do
    cont <- newName "contPrim"
    statement <- convert term cont
    pure
      Core.Do
        { location,
          name = cont,
          statement
        }
  convert term@Switch {location} = do
    cont <- newName "contSwitch"
    statement <- convert term cont
    pure
      Core.Do
        { location,
          name = cont,
          statement
        }
  convert term@Invoke {location} = do
    cont <- newName "contInvoke"
    statement <- convert term cont
    pure
      Core.Do
        { location,
          name = cont,
          statement
        }
  convert Label {..} = do
    statement <- convert term name
    pure
      Core.Do
        { location,
          name,
          statement
        }
  convert Goto {..} = do
    statement <- convert term name
    hole <- newName "hole"
    pure
      Core.Do
        { location,
          name = hole,
          statement
        }

instance (Error ToCoreError :> es) => Convert (Term Desugared Name) (Eff es Core.Consumer) where
  convert Var {..} = pure $ Core.Covar {..}
  convert term = throwError $ InvalidConsumer term.location term

instance Convert Literal Core.Literal where
  convert Int {..} = Core.Int {..}

-- This instance's `convert` takes a label as the return point of the statement.
instance (UniqueGen :> es, Error ToCoreError :> es) => Convert (Term Desugared Name) (Name -> Eff es Core.Statement) where
  convert Destruct {..} = \cont -> do
    term' <- convert term
    producers' <- traverse convert producers
    consumers' <- traverse convert consumers
    pure
      Core.Cut
        { location,
          producer = term',
          consumer =
            Core.Destruct
              { location,
                tag,
                producers = producers',
                consumers = consumers' <> [Core.Covar location cont]
              }
        }
  convert Match {..} = \cont -> do
    term' <- convert term
    clauses' <- for clauses \Clause {..} -> do
      let pattern' = convertPattern pattern
      statement <- convert term cont
      pure (pattern', statement)
    pure
      Core.Cut
        { location,
          producer = term',
          consumer = Core.Match {location, clauses = clauses'}
        }
    where
      convertPattern :: Pattern Desugared Name -> Core.Pattern
      convertPattern PConstruct {..} = Core.Pattern {..}
  convert Prim {..} = \cont -> do
    producers' <- traverse convert producers
    consumers' <- traverse convert consumers
    pure Core.Prim {location, tag, producers = producers', consumers = consumers' <> [Core.Covar location cont]}
  convert Switch {..} = \cont -> do
    producer <- convert term
    clauses <- for branches \(literal, term) -> do
      let literal' = convert literal
      statement <- convert term cont
      pure (literal', statement)
    statement <- convert defaultBranch cont
    pure Core.Switch {location, producer, clauses, statement}
  convert Invoke {..} = \cont -> do
    producers' <- traverse convert producers
    consumers' <- traverse convert consumers
    pure
      Core.Invoke
        { location,
          name,
          producers = producers',
          consumers = consumers' <> [Core.Covar location cont]
        }
  convert Goto {..} = \_ -> convert term name
  convert term = \cont -> do
    producer <- convert term
    pure Core.Cut {location = term.location, producer, consumer = Core.Covar {location = term.location, name = cont}}