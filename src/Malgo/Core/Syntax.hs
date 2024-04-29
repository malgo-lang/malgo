{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

-- | AST definitions for Core language
module Malgo.Core.Syntax
  ( module Malgo.Core.Syntax.Common,
    module Malgo.Core.Syntax.Unboxed,
    module Malgo.Core.Syntax.Atom,
    module Malgo.Core.Syntax.Expr,
    module Malgo.Core.Syntax.LocalDef,
    module Malgo.Core.Syntax.Case,
    Program (..),
    runDef,
    let_,
    bind,
    cast,
    callGraph,
  )
where

import Control.Lens (traverseOf, traversed, _3, _4)
import Data.Aeson (FromJSON, ToJSON)
import Data.Graph
import Data.HashSet qualified as HashSet
import Data.Store.TH
import Data.String.Conversions
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Effectful.Writer.Static.Local (Writer, runWriter, tell)
import Generic.Data
import Malgo.Core.Syntax.Atom
import Malgo.Core.Syntax.Case
import Malgo.Core.Syntax.Common
import Malgo.Core.Syntax.Expr
import Malgo.Core.Syntax.LocalDef
import Malgo.Core.Syntax.Unboxed
import Malgo.Core.Type
import Malgo.Id
import Malgo.MonadUniq
import Malgo.Prelude

-- | toplevel function definitions
data Program a = Program
  { topVars :: [(a, Type, Expr a)],
    topFuns :: [(a, [a], Type, Expr a)],
    extFuns :: [(Text, Type)]
  }
  deriving stock (Eq, Show, Functor, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via Generically (Program a)

makeStore ''Program

instance (Pretty a, Ord a) => Pretty (Program a) where
  pretty Program {..} =
    vcat $
      concat
        [ ["; variables"],
          map (\(v, t, e) -> parens $ sep ["define" <+> pretty v, pretty t, pretty e]) topVars,
          ["; functions"],
          map (\(f, ps, t, e) -> parens $ sep [sep ["define" <+> parens (sep $ map pretty $ f : ps), pretty t], pretty e]) topFuns,
          ["; externals"],
          map (\(f, t) -> parens $ sep ["extern", "%" <> pretty f, pretty t]) extFuns
        ]

instance HasExpr Program where
  expr f Program {..} =
    Program
      <$> traverseOf (traversed . _3) f topVars
      <*> traverseOf (traversed . _4) f topFuns
      <*> pure extFuns

runDef :: Eff (Writer (Endo (Expr (Meta Type))) : es) (Expr (Meta Type)) -> Eff es (Expr (Meta Type))
runDef m = uncurry (flip appEndo) <$> runWriter m

let_ :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo (Expr (Meta Type))) :> es) => Type -> Obj (Meta Type) -> Eff es (Atom (Meta Type))
let_ otype obj = do
  x <- withMeta otype <$> newTemporalId "let"
  tell $ Endo $ \e -> Let [LocalDef x otype obj] e
  pure (Var x)

bind :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo (Expr (Meta Type))) :> es) => Expr (Meta Type) -> Eff es (Atom (Meta Type))
bind (Atom a) = pure a
bind v = do
  x <- withMeta (typeOf v) <$> newTemporalId "d"
  tell $ Endo $ \e ->
    Assign x v e
  pure (Var x)

cast :: (State Uniq :> es, Reader ModuleName :> es, Writer (Endo (Expr (Meta Type))) :> es) => Type -> Expr (Meta Type) -> Eff es (Atom (Meta Type))
cast ty e
  | ty == typeOf e = bind e
  | otherwise = do
      v <- bind e
      x <- withMeta ty <$> newTemporalId "cast"
      tell $ Endo $ \e -> Assign x (Cast ty v) e
      pure (Var x)

-- `destruct` is convenient when treating types that have only one constructor.
-- For example, if we can write `let Foo x = v;` as the syntax sugar of `let x = v |> { Foo x -> x | _ -> error }`,
-- we can use `destruct` to support this syntax sugar.
-- But `let Foo x = v` style has some problem:
-- 1. Programmer must check whether the type can be treated that have only one constructor.
-- 2. There is more safe and convenenient way: `if let` in Rust.

-- destruct :: (MonadIO m, MonadReader env m, HasUniqSupply env UniqSupply) => Expr (Meta Type) -> Con -> DefBuilderT m [Atom (Meta Type)]
-- destruct val con@(Con _ ts) = do
--   vs <- traverse (newTemporalId "p") ts
--   DefBuilderT $ tell $ Endo $ \e -> Match val (Unpack con vs e :| [])
--   pure $ map Var vs

callGraph :: (Hashable a, Ord a) => Program a -> (Graph, Vertex -> (a, a, [a]), a -> Maybe Vertex)
callGraph Program {..} =
  let edges = map cgTopVar topVars <> map cgTopFun topFuns
   in graphFromEdges edges
  where
    cgTopVar (a, _, e) = (a, a, HashSet.toList $ callees e <> freevars e) -- Merge @callees@ and @freevars@ to avoid missing callees used as a closure.
    cgTopFun (a, ps, _, e) = (a, a, HashSet.toList $ HashSet.difference (callees e <> freevars e) (HashSet.fromList ps))