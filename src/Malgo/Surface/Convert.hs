module Malgo.Surface.Convert (toSyntax, ToSyntaxError (..)) where

import Data.Map qualified as Map
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Malgo.Location (Location)
import Malgo.Prelude
import Malgo.Surface
import Malgo.Syntax qualified as S
import Witherable (wither)

toSyntax :: [Definition Text] -> Either ToSyntaxError [S.Definition Text]
toSyntax definitions = runPureEff $ runErrorNoCallStack $ runReader @Env mempty do
  let toplevel =
        concatMap
          ( \case
              Definition {..} -> [(name, Procedure)]
              Data {..} -> map ((,Constructor) . fst) constructors
          )
          definitions
  local (Map.fromList toplevel <>) do
    wither resolve definitions

type Env = Map Text Kind

data Kind = Procedure | Constructor
  deriving (Eq, Show)

data ToSyntaxError = UndefinedVariable {location :: Location, name :: Text}
  deriving (Eq, Show)

class Resolve a r where
  resolve :: (Reader Env :> es, Error ToSyntaxError :> es) => a -> Eff es r

instance Resolve (Definition Text) (Maybe (S.Definition Text)) where
  resolve Definition {..} = do
    term <- resolve term
    pure $ Just S.Definition {..}
  resolve Data {} = pure Nothing

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
            Just Constructor -> pure S.Construct {tag = name, ..}
            Nothing ->
              pure
                S.Destruct
                  { location = varLocation,
                    term = S.Var {location = varLocation, name},
                    tag = "ap",
                    producers,
                    consumers
                  }
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
  resolve PConstruct{..} = do 
    params <- traverse resolve params
    returns <- traverse resolve returns
    pure S.PConstruct{..}
  resolve PVar{..} = pure S.PVar{..}

instance Resolve (Coclause Text) (S.Coclause Text) where
  resolve Coclause {..} = do
    copattern <- resolve copattern
    term <- resolve term
    pure S.Coclause {..}

instance Resolve (Copattern Text) (S.Copattern Text) where
  resolve Copattern {..} = pure S.Copattern {..}
