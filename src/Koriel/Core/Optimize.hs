{-# LANGUAGE TemplateHaskell #-}

module Koriel.Core.Optimize
  ( optimizeProgram,
    OptimizeOption (..),
    defaultOptimizeOption,
  )
where

import Control.Lens (At (at), makeFieldsNoPrefix, transformM, view)
import Control.Monad.Except
import Data.Data (Data)
import Data.Generics (gsize)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Koriel.Core.Alpha
import Koriel.Core.Flat
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Relude.Extra.Map (StaticMap (member))

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
    doSpecializeFunction :: Bool
  }

defaultOptimizeOption :: OptimizeOption
defaultOptimizeOption =
  OptimizeOption
    { doFoldVariable = True,
      doInlineConstructor = True,
      doEliminateUnusedLet = True,
      doInlineFunction = True,
      inlineThreshold = 10,
      doFoldRedundantCast = True,
      doFoldTrivialCall = True,
      doSpecializeFunction = False
    }

data OptimizeEnv = OptimizeEnv
  { uniqSupply :: UniqSupply,
    moduleName :: ModuleName,
    debugMode :: Bool,
    option :: OptimizeOption
  }

makeFieldsNoPrefix ''OptimizeEnv

-- | Apply a monadic function n times.
times :: (Monad m, Eq t) => Int -> (t -> m t) -> t -> m t
times 0 _ x = pure x
times n f x
  | n > 0 = times (n - 1) f =<< f x
  | otherwise = error $ show n <> " must be a natural number"

-- | Optimize a program
optimizeProgram ::
  MonadIO m =>
  UniqSupply ->
  ModuleName ->
  Bool ->
  OptimizeOption ->
  Program (Id Type) ->
  m (Program (Id Type))
optimizeProgram uniqSupply moduleName debugMode option Program {..} = runReaderT ?? OptimizeEnv {..} $ do
  state <- execStateT ?? CallInlineEnv mempty $ for_ topFuns $ \(name, ps, t, e) -> checkInlinable $ LocalDef name t (Fun ps e)
  topVars <- traverse (\(n, t, e) -> (n,t,) <$> optimizeExpr state e) topVars
  topFuns <- traverse (\(n, ps, t, e) -> (n,ps,t,) <$> optimizeExpr (CallInlineEnv $ HashMap.delete n state.inlinableMap) e) topFuns
  pure $ Program {..}

optimizeExpr :: (MonadReader OptimizeEnv f, MonadIO f) => CallInlineEnv -> Expr (Id Type) -> f (Expr (Id Type))
optimizeExpr state expr = do
  option <- asks (.option)
  5 `times` opt option $ expr
  where
    opt option = do
      pure
        >=> runOpt option.doFoldVariable foldVariable
        >=> runOpt option.doInlineConstructor (usingReaderT mempty . inlineConstructor)
        >=> runOpt option.doEliminateUnusedLet eliminateUnusedLet
        >=> runOpt option.doInlineFunction (flip evalStateT state . inlineFunction)
        >=> runOpt option.doFoldRedundantCast foldRedundantCast
        >=> runOpt option.doFoldTrivialCall foldTrivialCall
        >=> runOpt option.doSpecializeFunction specializeFunction
        >=> runFlat . flatExpr
    runOpt :: Monad m => Bool -> (Expr (Id Type) -> m (Expr (Id Type))) -> Expr (Id Type) -> m (Expr (Id Type))
    runOpt flag f =
      if flag
        then f
        else pure

-- | (let ((f (fun ps body))) (f as)) = body[as/ps]
foldTrivialCall :: (MonadIO f, MonadReader OptimizeEnv f) => Expr (Id Type) -> f (Expr (Id Type))
foldTrivialCall = transformM \case
  Let [LocalDef f _ (Fun ps body)] (Call (Var f') as) | f == f' -> do
    uniqSupply <- asks (.uniqSupply)
    alpha body AlphaEnv {uniqSupply, subst = HashMap.fromList $ zip ps as}
  x -> pure x

newtype CallInlineEnv = CallInlineEnv
  { inlinableMap :: HashMap (Id Type) ([Id Type], Expr (Id Type))
  }

-- | Inline a function call.
inlineFunction :: (MonadState CallInlineEnv f, MonadReader OptimizeEnv f) => Expr (Id Type) -> f (Expr (Id Type))
inlineFunction =
  transformM \case
    Call (Var f) xs -> lookupCallInline (Call . Var) f xs
    CallDirect f xs -> lookupCallInline CallDirect f xs
    Let ds e -> do
      traverse_ checkInlinable ds
      pure $ Let ds e
    x -> pure x

checkInlinable :: (MonadReader OptimizeEnv m, MonadState CallInlineEnv m) => LocalDef (Id Type) -> m ()
checkInlinable (LocalDef f _ (Fun ps v)) = do
  threshold <- asks (.option.inlineThreshold)
  -- ノードの数がthreshold以下ならインライン展開する
  -- v <- inlineFunction v
  let isInlinable = threshold >= gsize v
  when isInlinable $ do
    modify $ \e -> e {inlinableMap = HashMap.insert f (ps, v) e.inlinableMap}
checkInlinable _ = pass

-- | Lookup a function in the inlinable map.
lookupCallInline ::
  (MonadReader OptimizeEnv m, MonadState CallInlineEnv m) =>
  (Id Type -> [Atom (Id Type)] -> Expr (Id Type)) ->
  Id Type ->
  [Atom (Id Type)] ->
  m (Expr (Id Type))
lookupCallInline call f as = do
  f' <- gets $ (.inlinableMap) >>> HashMap.lookup f
  case f' of
    Just (ps, v) ->
      -- v[as/ps]
      pure $ foldl' (\e (x, a) -> Assign x (Atom a) e) v $ zip ps as
    Nothing -> pure $ call f as

type InlineConstructorMap = HashMap (Id Type) (Con, [Atom (Id Type)])

-- | Inline simple pattern match and pack.
inlineConstructor :: MonadReader InlineConstructorMap m => Expr (Id Type) -> m (Expr (Id Type))
inlineConstructor =
  transformM \case
    Let ds e -> do
      local (mconcat (map toPackInlineMap ds) <>) $ pure $ Let ds e
    Match (Atom (Var v)) [Unpack con xs body] -> do
      view (at v) >>= \case
        Just (con', as) | con == con' -> pure $ build xs as body
        _ -> pure $ Destruct (Var v) con xs body
    Destruct (Var v) con xs body -> do
      view (at v) >>= \case
        Just (con', as) | con == con' -> pure $ build xs as body
        _ -> pure $ Destruct (Var v) con xs body
    e -> pure e
  where
    toPackInlineMap (LocalDef v _ (Pack _ con as)) = one (v, (con, as))
    toPackInlineMap _ = mempty
    build (x : xs) (a : as) body = Assign x (Atom a) (build xs as body)
    build _ _ body = body

-- | Remove variable binding if that variable is an alias of another variable.
foldVariable :: (Eq a, Monad f, Data a) => Expr a -> f (Expr a)
foldVariable = transformM
  \case
    Match (Atom a) [Bind x _ e] -> pure $ replaceOf atom (Var x) a e
    Assign x (Atom a) e -> pure $ replaceOf atom (Var x) a e
    x -> pure x

-- | Remove unused let bindings
-- Let bindings only bind expressions that allocate memory. So we can remove unused let bindings safely.
eliminateUnusedLet :: (Monad f, Hashable a, Data a) => Expr (Id a) -> f (Expr (Id a))
eliminateUnusedLet =
  transformM \case
    Let ds e -> do
      -- Reachable variables from 'v'
      let gamma = map (\(LocalDef v _ o) -> (v, HashSet.delete v $ freevars o)) ds
      if any (\(LocalDef v _ _) -> reachable 100 gamma v $ freevars e) ds
        then pure $ Let ds e
        else pure e
    e -> pure e
  where
    reachable limit gamma v fvs
      | limit <= (0 :: Int) = True
      | idIsExternal v = True
      | v `member` fvs = True
      | otherwise =
          -- Add gamma[fv] to fvs
          let fvs' = fvs <> mconcat (mapMaybe (List.lookup ?? gamma) $ HashSet.toList fvs)
           in fvs /= fvs' && reachable limit gamma v fvs'

-- | Remove a cast if it is redundant.
foldRedundantCast :: (HasType a, Data a, Monad f) => Expr a -> f (Expr a)
foldRedundantCast =
  transformM \case
    Cast t e
      | typeOf e == t -> pure (Atom e)
      | otherwise -> pure (Cast t e)
    e -> pure e

-- | Specialize a function which is casted to a specific type.
-- TODO: ベンチマーク
specializeFunction :: (MonadIO m, MonadReader OptimizeEnv m) => Expr (Id Type) -> m (Expr (Id Type))
specializeFunction =
  transformM \case
    Cast (pts' :-> rt') f -> case typeOf f of
      pts :-> _
        | length pts' == length pts -> do
            f' <- newInternalId "cast_opt" (pts' :-> rt')
            ps' <- traverse (newInternalId "p") pts'
            v' <- runDef do
              ps <- zipWithM cast pts $ map (Atom . Var) ps'

              -- If `f` is always a unknown function because it appears in `cast`.
              r <- bind (Call f ps)
              pure $ Cast rt' r
            pure (Let [LocalDef f' (pts' :-> rt') $ Fun ps' v'] (Atom $ Var f'))
        | otherwise -> error "specializeFunction: invalid cast"
      _ -> pure (Cast (pts' :-> rt') f)
    e -> pure e