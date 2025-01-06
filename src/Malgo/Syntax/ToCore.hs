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

toCore :: (UniqueGen :> es, Error ToCoreError :> es) => [Definition Name] -> Eff es [Core.Definition]
toCore definitions = do
  traverse convert definitions

class Convert a r where
  convert :: (UniqueGen :> es, Error ToCoreError :> es) => a -> Eff es r

data ToCoreError = InvalidConsumer {location :: Location, term :: Term Name}
  deriving (Show)

instance Convert (Definition Name) Core.Definition where
  convert Definition {..} = do
    result <- newName "result"
    statementBuilder <- convert term
    statement <- statementBuilder result
    pure
      $ Core.Definition
        { name,
          params,
          returns = returns <> [result],
          statement
        }

instance Convert (Term Name) Core.Producer where
  convert Var {..} = pure $ Core.Var {..}
  convert Literal {..} = do
    literal' <- convert literal
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
    cont <- newName "contComatch"
    clauses <- for coclauses \Coclause {..} -> do
      let copattern' = convertCopattern cont copattern
      statementBuilder <- convert term
      statement <- statementBuilder cont
      pure (copattern', statement)
    pure $ Core.Comatch {location, clauses}
    where
      convertCopattern cont Copattern {..} =
        Core.Copattern
          { tag,
            params,
            returns = returns <> [cont]
          }
  convert Destruct {..} = do
    cont <- newName "contDestruct"
    term' <- convert term
    producers' <- traverse convert producers
    consumers' <- traverse convert consumers
    pure
      Core.Do
        { location,
          name = cont,
          statement =
            Core.Cut
              { location,
                producer = term',
                consumer =
                  Core.Destruct
                    { location,
                      tag,
                      producers = producers',
                      consumers = consumers' <> [Core.Label location cont]
                    }
              }
        }
  convert Match {..} = do
    cont <- newName "contMatch"
    term' <- convert term
    clauses' <- for clauses \Clause {..} -> do
      let pattern' = convertPattern pattern
      statementBuilder <- convert term
      statement <- statementBuilder cont
      pure (pattern', statement)
    pure
      Core.Do
        { location,
          name = cont,
          statement =
            Core.Cut
              { location,
                producer = term',
                consumer =
                  Core.Match
                    { location,
                      clauses = clauses'
                    }
              }
        }
    where
      convertPattern Pattern {..} = Core.Pattern {..}
  convert term@Prim {location} = do
    cont <- newName "contPrim"
    statementBuilder <- convert term
    statement <- statementBuilder cont
    pure
      Core.Do
        { location,
          name = cont,
          statement
        }
  convert term@Switch {location} = do
    cont <- newName "contSwitch"
    statementBuilder <- convert term
    statement <- statementBuilder cont
    pure
      Core.Do
        { location,
          name = cont,
          statement
        }
  convert term@Invoke {location} = do
    cont <- newName "contInvoke"
    statementBuilder <- convert term
    statement <- statementBuilder cont
    pure
      Core.Do
        { location,
          name = cont,
          statement
        }
  convert Label {..} = do
    statementBuilder <- convert term
    statement <- statementBuilder name
    pure
      Core.Do
        { location,
          name,
          statement
        }
  convert Goto {..} = do
    statementBuilder <- convert term
    statement <- statementBuilder name
    hole <- newName "hole"
    pure
      Core.Do
        { location,
          name = hole,
          statement
        }

instance Convert (Term Name) Core.Consumer where
  convert Var {..} = pure $ Core.Label {..}
  convert term = throwError $ InvalidConsumer term.location term

instance Convert Literal Core.Literal where
  convert Int {..} = pure $ Core.Int {..}

instance (UniqueGen :> es, Error ToCoreError :> es) => Convert (Term Name) (Name -> Eff es Core.Statement) where
  convert Prim {..} = do
    producers' <- traverse convert producers
    consumers' <- traverse convert consumers
    pure \name -> pure Core.Prim {location, tag, producers = producers', consumers = consumers' <> [Core.Label location name]}
  convert Switch {..} = pure \name -> do
    producer <- convert term
    clauses <- for branches \(literal, term) -> do
      literal' <- convert literal
      statementBuilder <- convert term
      statement <- statementBuilder name
      pure (literal', statement)
    statementBuilder <- convert defaultBranch
    statement <- statementBuilder name
    pure Core.Switch {location, producer, clauses, statement}
  convert Invoke {..} = pure \cont -> do
    producers' <- traverse convert producers
    consumers' <- traverse convert consumers
    pure
      Core.Invoke
        { location,
          name,
          producers = producers',
          consumers = consumers' <> [Core.Label location cont]
        }
  convert term = do
    producer <- convert term
    pure \name -> pure Core.Cut {location = term.location, producer, consumer = Core.Label {location = term.location, name}}