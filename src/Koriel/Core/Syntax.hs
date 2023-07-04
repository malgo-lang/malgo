{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

-- | AST definitions for Koriel language
module Koriel.Core.Syntax
  ( module Koriel.Core.Syntax.Common,
    module Koriel.Core.Syntax.Unboxed,
    module Koriel.Core.Syntax.Atom,
    module Koriel.Core.Syntax.Expr,
    module Koriel.Core.Syntax.LocalDef,
    module Koriel.Core.Syntax.Case,
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
import Data.Binary (Binary)
import Data.Graph
import Data.HashSet qualified as HashSet
import Data.String.Conversions
import Generic.Data
import Koriel.Core.Syntax.Atom
import Koriel.Core.Syntax.Case
import Koriel.Core.Syntax.Common
import Koriel.Core.Syntax.Expr
import Koriel.Core.Syntax.LocalDef
import Koriel.Core.Syntax.Unboxed
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty

-- | toplevel function definitions
data Program a = Program
  { topVars :: [(a, Type, Expr a)],
    topFuns :: [(a, [a], Type, Expr a)],
    extFuns :: [(Text, Type)]
  }
  deriving stock (Eq, Show, Functor, Generic)
  deriving anyclass (Binary, ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via Generically (Program a)

instance (Pretty a, Ord a) => Pretty (Program a) where
  pPrint Program {..} =
    vcat
      $ concat
        [ ["; variables"],
          map (\(v, t, e) -> parens $ sep ["define" <+> pPrint v, pPrint t, pPrint e]) topVars,
          ["; functions"],
          map (\(f, ps, t, e) -> parens $ sep [sep ["define" <+> parens (sep $ map pPrint $ f : ps), pPrint t], pPrint e]) topFuns,
          ["; externals"],
          map (\(f, t) -> parens $ sep ["extern", "%" <> pPrint f, pPrint t]) extFuns
        ]

instance HasExpr Program where
  expr f Program {..} =
    Program
      <$> traverseOf (traversed . _3) f topVars
      <*> traverseOf (traversed . _4) f topFuns
      <*> pure extFuns

newtype DefBuilderT m a = DefBuilderT {unDefBuilderT :: WriterT (Endo (Expr (Id Type))) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState s, MonadReader r)

runDef :: (Functor f) => DefBuilderT f (Expr (Id Type)) -> f (Expr (Id Type))
runDef m = uncurry (flip appEndo) <$> runWriterT m.unDefBuilderT

let_ :: (MonadIO m, MonadReader env m, HasUniqSupply env, HasModuleName env) => Type -> Obj (Id Type) -> DefBuilderT m (Atom (Id Type))
let_ otype obj = do
  x <- newTemporalId "let" otype
  DefBuilderT $ tell $ Endo $ \e -> Let [LocalDef x otype obj] e
  pure (Var x)

bind :: (MonadIO m, MonadReader env m, HasUniqSupply env, HasModuleName env) => Expr (Id Type) -> DefBuilderT m (Atom (Id Type))
bind (Atom a) = pure a
bind v = do
  x <- newTemporalId "d" (typeOf v)
  DefBuilderT $ tell $ Endo $ \e ->
    Assign x v e
  pure (Var x)

cast :: (MonadIO m, MonadReader env m, HasUniqSupply env, HasModuleName env) => Type -> Expr (Id Type) -> DefBuilderT m (Atom (Id Type))
cast ty e
  | ty == typeOf e = bind e
  | otherwise = do
      v <- bind e
      x <- newTemporalId "cast" ty
      DefBuilderT $ tell $ Endo $ \e -> Assign x (Cast ty v) e
      pure (Var x)

-- `destruct` is convenient when treating types that have only one constructor.
-- For example, if we can write `let Foo x = v;` as the syntax sugar of `let x = v |> { Foo x -> x | _ -> error }`,
-- we can use `destruct` to support this syntax sugar.
-- But `let Foo x = v` style has some problem:
-- 1. Programmer must check whether the type can be treated that have only one constructor.
-- 2. There is more safe and convenenient way: `if let` in Rust.

-- destruct :: (MonadIO m, MonadReader env m, HasUniqSupply env UniqSupply) => Expr (Id Type) -> Con -> DefBuilderT m [Atom (Id Type)]
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