{-# LANGUAGE TypeFamilies #-}

module Malgo.Syntax.ToCore where

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
    term' <- convert term
    pure
      $ Core.Definition
        { name,
          params,
          returns = returns <> [result],
          statement =
            Core.Cut location term'
              $ Core.Label location result
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
    cont <- newName "cont"
    clauses <- for coclauses \Coclause {..} -> do
      let copattern' = convertCopattern cont copattern
      term' <- convert term
      pure (copattern', Core.Cut location term' (Core.Label location cont))
    pure $ Core.Comatch {location, clauses}
    where
      convertCopattern cont Copattern {..} =
        Core.Copattern
          { tag,
            params,
            returns = returns <> [cont]
          }
  convert Destruct {..} = do
    term' <- convert term
    producers' <- traverse convert producers
    consumers' <- traverse convert consumers
    doCut location term' \cont ->
      Core.Destruct
        { location,
          tag,
          producers = producers',
          consumers = consumers' <> [Core.Label location cont]
        }
  convert Match {..} = do
    term' <- convert term
    clauses' <- for clauses \Clause {..} -> do
      let pattern' = convertPattern pattern
      term' <- convert term
      pure (pattern', term')
    doCut location term' \cont ->
      Core.Match
        { location,
          clauses =
            map
              ( \(pattern, producer) ->
                  ( pattern,
                    Core.Cut location producer (Core.Label location cont)
                  )
              )
              clauses'
        }
    where
      convertPattern Pattern {..} = Core.Pattern {..}
  convert Prim {..} = do
    producers' <- traverse convert producers
    consumers' <- traverse convert consumers
    cont <- newName "cont"
    pure
      Core.Do
        { location,
          name = cont,
          statement =
            Core.Prim
              { location,
                tag,
                producers = producers',
                consumers = consumers' <> [Core.Label location cont]
              }
        }
  convert Switch {..} = do
    term' <- convert term
    branches' <- for branches \(literal, term) -> do
      literal' <- convert literal
      term' <- convert term
      pure (literal', term')
    defaultBranch' <- convert defaultBranch
    cont <- newName "cont"
    pure
      Core.Do
        { location,
          name = cont,
          statement =
            Core.Switch
              { location,
                producer = term',
                clauses =
                  map
                    ( \(literal, producer) ->
                        ( literal,
                          Core.Cut location producer (Core.Label location cont)
                        )
                    )
                    branches',
                statement = Core.Cut location defaultBranch' (Core.Label location cont)
              }
        }
  convert Invoke {..} = do
    producers' <- traverse convert producers
    consumers' <- traverse convert consumers
    cont <- newName "cont"
    pure
      Core.Do
        { location,
          name = cont,
          statement =
            Core.Invoke
              { location,
                name,
                producers = producers',
                consumers = consumers' <> [Core.Label location cont]
              }
        }
  convert Label {..} = do
    term' <- convert term
    pure
      Core.Do
        { location,
          name,
          statement =
            Core.Cut
              { location,
                producer = term',
                consumer = Core.Label location name
              }
        }
  convert Goto {..} = do
    term' <- convert term
    hole <- newName "hole"
    pure
      Core.Do
        { location,
          name = hole,
          statement =
            Core.Cut
              { location,
                producer = term',
                consumer = Core.Label location name
              }
        }

doCut :: (UniqueGen :> es) => Location -> Core.Producer -> (Name -> Core.Consumer) -> Eff es Core.Producer
doCut location producer consumerBuilder = do
  cont <- newName "cont"
  pure
    Core.Do
      { location,
        name = cont,
        statement =
          Core.Cut
            { location,
              producer,
              consumer = consumerBuilder cont
            }
      }

instance Convert (Term Name) Core.Consumer where
  convert Var {..} = pure $ Core.Label {..}
  convert term = throwError $ InvalidConsumer term.location term

instance Convert Literal Core.Literal where
  convert Int {..} = pure $ Core.Int {..}