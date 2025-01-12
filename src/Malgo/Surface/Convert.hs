module Malgo.Surface.Convert (toSyntax, ToSyntaxError (..)) where

import Data.Char (isUpper)
import Data.Map qualified as Map
import Data.Text qualified as T
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Malgo.Location (Location)
import Malgo.Prelude
import Malgo.Surface
import Malgo.Syntax qualified as S

toSyntax :: [Definition Text] -> Either ToSyntaxError [S.Definition Text]
toSyntax definitions = runPureEff $ runErrorNoCallStack $ runReader @Env mempty do
  let procedures = map (\Definition {name} -> (name, Procedure)) definitions
  local (Map.fromList procedures <>) do
    traverse resolve $ (definitions :: [Definition Text])

type Env = Map Text Kind

data Kind = Procedure
  deriving (Eq, Show)

data ToSyntaxError = UndefinedVariable {location :: Location, name :: Text}
  deriving (Eq, Show)

class Resolve a r where
  resolve :: (Reader Env :> es, Error ToSyntaxError :> es) => a -> Eff es r

instance Resolve (Definition Text) (S.Definition Text) where
  resolve Definition {..} = do
    term <- resolve term
    pure S.Definition {..}

instance Resolve (Term Text) (S.Term Text) where
  resolve Var {..} = pure S.Var {..}
  resolve Literal {..} = do
    literal <- resolve literal
    pure S.Literal {..}
  resolve
    Apply
      { location,
        term = Var {location = varLocation, ..},
        producers,
        consumers
      }
      | name == "prim" = do
          tag <- case producers of
            (Var {name} : _) -> pure name
            _ -> error "invalid prim"
          producers <- traverse resolve $ drop 1 producers
          consumers <- traverse resolve consumers
          pure S.Prim {..}
      | otherwise = do
          producers <- traverse resolve producers
          consumers <- traverse resolve consumers
          kind <- asks (Map.lookup name)
          case kind of
            Just Procedure -> pure S.Invoke {..}
            Nothing ->
              if isConstructorTag name
                then pure S.Construct {tag = name, ..}
                else
                  pure
                    S.Destruct
                      { location = varLocation,
                        term = S.Var {location = varLocation, name},
                        tag = "ap",
                        producers,
                        consumers
                      }
      where
        isConstructorTag name = case T.uncons name of
          Just (c, _) -> isUpper c
          Nothing -> False
  resolve Apply {..} = do
    term <- resolve term
    producers <- traverse resolve producers
    consumers <- traverse resolve consumers
    pure
      S.Destruct
        { location,
          term,
          tag = "ap",
          producers,
          consumers
        }
  resolve Comatch {..} = do
    coclauses <- traverse resolve coclauses
    pure S.Comatch {..}
  resolve Destruct {..} = do
    term <- resolve term
    producers <- traverse resolve producers
    consumers <- traverse resolve consumers
    pure S.Destruct {..}
  resolve Match {..} = do
    term <- resolve term
    clauses <- traverse resolve clauses
    pure S.Match {..}
  resolve Switch {..} = do
    term <- resolve term
    branches <- traverse (\(literal, term) -> (,) <$> resolve literal <*> resolve term) branches
    defaultBranch <- resolve defaultBranch
    pure S.Switch {..}
  resolve Label {..} = do
    term <- resolve term
    pure S.Label {..}
  resolve Goto {..} = do
    term <- resolve term
    pure S.Goto {..}

instance Resolve Literal S.Literal where
  resolve Int {..} = pure S.Int {..}

instance Resolve (Clause Text) (S.Clause Text) where
  resolve Clause {..} = do
    pattern <- resolve pattern
    term <- resolve term
    pure S.Clause {..}

instance Resolve (Pattern Text) (S.Pattern Text) where
  resolve Pattern {..} = pure S.Pattern {..}

instance Resolve (Coclause Text) (S.Coclause Text) where
  resolve Coclause {..} = do
    copattern <- resolve copattern
    term <- resolve term
    pure S.Coclause {..}

instance Resolve (Copattern Text) (S.Copattern Text) where
  resolve Copattern {..} = pure S.Copattern {..}
