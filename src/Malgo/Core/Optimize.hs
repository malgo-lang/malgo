module Malgo.Core.Optimize
  ( optimizeProgram,
    OptimizeOption (..),
    defaultOptimizeOption,
  )
where

import Control.Lens (lengthOf, transform, transformM, view, _1)
import Data.Graph qualified as Graph
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask, asks, local, runReader)
import Effectful.State.Static.Local (State, evalState, execState, get, modify)
import Effectful.State.Static.Local qualified as L
import Malgo.Core.Alpha
import Malgo.Core.Flat
import Malgo.Core.Syntax
import Malgo.Core.Type
import Malgo.Id
import Malgo.Module
import Malgo.MonadUniq
import Malgo.Prelude
import Witherable (ordNub)

-- | 'OptimizeOption' is a set of options for the optimizer.
--  If the option is 'True', the optimizer will apply the optimization.
data OptimizeOption = OptimizeOption
  { doFoldVariable :: Bool,
    doInlineConstructor :: Bool,
    doEliminateUnusedLet :: Bool,
    doInlineFunction :: Bool,
    -- | Threshold of the size of functions to be inlined.
    inlineThreshold :: Int,
    doFoldRedundantCast :: Bool,
    doFoldTrivialCall :: Bool,
    doSpecializeFunction :: Bool,
    doRemoveNoopDestruct :: Bool
  }

defaultOptimizeOption :: OptimizeOption
defaultOptimizeOption =
  OptimizeOption
    { doFoldVariable = True,
      doInlineConstructor = True,
      doEliminateUnusedLet = True,
      doInlineFunction = True,
      inlineThreshold = 15,
      doFoldRedundantCast = True,
      doFoldTrivialCall = True,
      doSpecializeFunction = False,
      doRemoveNoopDestruct = True
    }

-- | Apply a monadic function n times.
times :: (Monad m) => Int -> (t -> m t) -> t -> m t
times 0 _ x = pure x
times n f x
  | n > 0 = times (n - 1) f =<< f x
  | otherwise = error $ show n <> " must be a natural number"
{-# SCC times #-}

-- | Optimize a program
optimizeProgram ::
  ( Reader OptimizeOption :> es,
    Reader ModuleName :> es,
    State Uniq :> es
  ) =>
  Program (Meta Type) ->
  Eff es (Program (Meta Type))
optimizeProgram Program {..} = do
  state <-
    {-# SCC "buildState" #-}
    execState mempty do
      {-# SCC "checkInlinable_topFuns" #-} for_ topFuns $ \(name, ps, t, e) -> checkInlinable $ LocalDef name t (Fun ps e)
      {-# SCC "checkInlinable_topVars" #-}
        for_ topVars $ \case
          (name, t, Let [LocalDef f _ (Fun ps e)] (Atom (Var v))) | f == v -> checkInlinable $ LocalDef name t (Fun ps e)
          _ -> pass
  topVars <- {-# SCC "optimizeExpr_topVars" #-} traverse (\(n, t, e) -> (n,t,) <$> optimizeExpr state e) topVars
  topFuns <- {-# SCC "optimizeExpr_topFuns" #-} traverse (\(n, ps, t, e) -> (n,ps,t,) <$> optimizeExpr state e) topFuns

  -- Remove all unused toplevel functions and variables.
  -- If a global definition is external and defined in the current module, it cannot be removed.
  -- Otherwise, delete it if it is not reachable from above definitions.
  moduleName <- ask @ModuleName
  let roots = {-# SCC "roots" #-} filter (\x -> idIsExternal x.id && x.id.moduleName == moduleName) $ map (view _1) topFuns <> map (view _1) topVars
  let (graph, _, toVertex) = {-# SCC "callGraph" #-} callGraph Program {..}
  let reachableFromMain = {-# SCC "reachable" #-} ordNub $ concatMap (toVertex >>> Maybe.fromJust >>> Graph.reachable graph) roots

  let isUsed a = {-# SCC "used" #-} toVertex a `elem` map Just reachableFromMain

  topVars <- {-# SCC "filter_topVars" #-} pure $ filter (\(n, _, _) -> isUsed n) topVars
  topFuns <- {-# SCC "filter_topFuns" #-} pure $ filter (\(n, _, _, _) -> isUsed n) topFuns
  pure $ {-# SCC "buildProgram" #-} Program {..}

optimizeExpr :: (Reader OptimizeOption :> es, Reader ModuleName :> es, State Uniq :> es) => CallInlineEnv -> Expr (Meta Type) -> Eff es (Expr (Meta Type))
optimizeExpr state expr = do
  option <- ask @OptimizeOption
  5 `times` opt option $ expr
  where
    opt option e = do
      e <- {-# SCC "foldVariable" #-} runOpt option.doFoldVariable foldVariable e
      e <- {-# SCC "inlineConstructor" #-} runOpt option.doInlineConstructor (runReader @InlineConstructorMap mempty . inlineConstructor) e
      e <- {-# SCC "eliminateUnusedLet" #-} runOpt option.doEliminateUnusedLet eliminateUnusedLet e
      e <- {-# SCC "inlineFunction" #-} runOpt option.doInlineFunction (evalState state . inlineFunction) e
      e <- {-# SCC "foldRedundantCast" #-} runOpt option.doFoldRedundantCast foldRedundantCast e
      e <- {-# SCC "foldTrivialCall" #-} runOpt option.doFoldTrivialCall foldTrivialCall e
      e <- {-# SCC "removeNoopDestruct" #-} runOpt option.doRemoveNoopDestruct (pure . removeNoopDestruct) e
      {-# SCC "normalizeExpr" #-} normalizeExpr e
    runOpt flag f =
      if flag
        then f
        else pure

-- | Remove variable binding if that variable is an alias of another variable.
foldVariable :: (Eq a, Monad f) => Expr a -> f (Expr a)
foldVariable = transformM
  \case
    Match (Atom a) [Bind x _ e] -> pure $ replaceOf atom (Var x) a e
    -- (= x a e) -> e[x := a] is a valid transformation in 'foldVariable'.
    -- But this transformation is not necessary, because 'Malgo.Core.Flat' and 'Malgo.Core.Lint' guarantee that '=' never binds an atom.
    -- Assign x (Atom a) e -> pure $ replaceOf atom (Var x) a e
    x -> pure x

type InlineConstructorMap = Map (Meta Type) (Con, [Atom (Meta Type)])

-- | Inline simple pattern match and pack.
inlineConstructor :: (Reader InlineConstructorMap :> es) => Expr (Meta Type) -> Eff es (Expr (Meta Type))
inlineConstructor =
  transformM \case
    Let ds e -> do
      local (mconcat (map toPackInlineMap ds) <>) $ pure $ Let ds e
    Match (Atom (Var v)) [Unpack con xs body] -> do
      asks (Map.lookup v) >>= \case
        Just (con', as) | con == con' -> pure $ build xs as body
        _ -> pure $ Destruct (Var v) con xs body
    Destruct (Var v) con xs body -> do
      asks (Map.lookup v) >>= \case
        Just (con', as) | con == con' -> pure $ build xs as body
        _ -> pure $ Destruct (Var v) con xs body
    e -> pure e
  where
    toPackInlineMap (LocalDef v _ (Pack _ con as)) = Map.singleton v (con, as)
    toPackInlineMap _ = mempty
    build (x : xs) (a : as) body = Assign x (Atom a) (build xs as body)
    build _ _ body = body

-- | Remove unused let bindings
-- Let bindings only bind expressions that allocate memory. So we can remove unused let bindings safely.
eliminateUnusedLet :: (Monad f, Ord a) => Expr (Meta a) -> f (Expr (Meta a))
eliminateUnusedLet =
  transformM \case
    Let ds e -> do
      -- Reachable variables from 'v'
      let gamma = map (\(LocalDef v _ o) -> (v, Set.delete v $ freevars o)) ds
      if any (\(LocalDef v _ _) -> reachable 100 gamma v $ freevars e) ds
        then pure $ Let ds e
        else pure e
    e -> pure e
  where
    reachable limit gamma v fvs
      | limit <= (0 :: Int) = True
      | idIsExternal v.id = True
      | Set.member v fvs = True
      | otherwise =
          -- Add gamma[fv] to fvs
          let fvs' = fvs <> mconcat (mapMaybe (List.lookup ?? gamma) $ Set.toList fvs)
           in fvs /= fvs' && reachable limit gamma v fvs'

-- TODO: Merge with OptimizeEnv
type CallInlineEnv = Map (Meta Type) ([Meta Type], Expr (Meta Type))

-- | Inline a function call.
inlineFunction :: (Reader OptimizeOption :> es, L.State CallInlineEnv :> es, Reader ModuleName :> es, State Uniq :> es) => Expr (Meta Type) -> Eff es (Expr (Meta Type))
inlineFunction =
  transformM
    ( \case
        Let ds e -> do
          traverse_ checkInlinable ds
          pure $ Let ds e
        e -> pure e
    )
    >=> transformM \case
      Call (Var f) xs -> lookupCallInline (Call . Var) f xs
      CallDirect f xs -> lookupCallInline CallDirect f xs
      x -> pure x

checkInlinable ::
  (Reader OptimizeOption :> es, L.State CallInlineEnv :> es) =>
  LocalDef (Meta Type) ->
  Eff es ()
checkInlinable (LocalDef f _ (Fun ps v)) = do
  threshold <- asks @OptimizeOption (.inlineThreshold)
  -- atomの数がthreshold以下ならインライン展開する
  -- TODO: 再帰関数かどうかコールグラフを作って判定する
  let isInlinable =
        threshold
          >= lengthOf atom v
          && not (f `Set.member` freevars v)
          && not (f `Set.member` callees v)
  when isInlinable $ do
    modify @CallInlineEnv $ Map.insert f (ps, v)
checkInlinable _ = pass

-- | Lookup a function in the inlinable map.
lookupCallInline ::
  ( L.State CallInlineEnv :> es,
    Reader ModuleName :> es,
    State Uniq :> es
  ) =>
  (Meta Type -> [Atom (Meta Type)] -> Expr (Meta Type)) ->
  Meta Type ->
  [Atom (Meta Type)] ->
  Eff es (Expr (Meta Type))
lookupCallInline call f as = do
  env <- get @CallInlineEnv
  case Map.lookup f env of
    Just (ps, v) -> do
      -- v[as/ps]
      -- Test failed in Lint : pure $ foldl' (\e (x, a) -> Assign x (Atom a) e) v $ zip ps as
      -- use Alpha.alpha
      let subst = Map.fromList $ zip ps as
      alpha subst v
    Nothing -> pure $ call f as

-- | Remove a cast if it is redundant.
-- TODO: switch and match that ends with a cast can be simplified.
foldRedundantCast :: (Monad f) => Expr (Meta Type) -> f (Expr (Meta Type))
foldRedundantCast =
  transformM \case
    -- (= x (cast t a) (= y (cast t' x)) e) -> (= y (cast t' a) e)
    Assign x (Cast _ a) (Assign y (Cast t' (Var x')) e)
      | x == x' && not (x `Set.member` freevars e) -> pure $ Assign y (Cast t' a) e
    Cast t e
      | typeOf e == t -> pure (Atom e)
      | otherwise -> pure (Cast t e)
    e -> pure e

-- | (let ((f (fun ps body))) (f as)) = body[as/ps]
foldTrivialCall :: (Reader ModuleName :> es, State Uniq :> es) => Expr (Meta Type) -> Eff es (Expr (Meta Type))
foldTrivialCall = transformM \case
  Let [LocalDef f _ (Fun ps body)] (Call (Var f') as) | f == f' -> do
    alpha (Map.fromList $ zip ps as) body
  x -> pure x

-- | Remove `destruct` if it does not bind any variables.
removeNoopDestruct :: Expr (Meta a) -> Expr (Meta a)
removeNoopDestruct =
  transform \case
    Destruct _ _ [] e -> e
    e -> e
