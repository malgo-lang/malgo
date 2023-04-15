{-# LANGUAGE TemplateHaskell #-}

module Koriel.Core.Optimize
  ( optimizeProgram,
    OptimizeOption (..),
    defaultOptimizeOption,
  )
where

import Control.Lens (At (at), makeFieldsNoPrefix, traverseOf, traversed, view, _2)
import Control.Monad.Except
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
        >=> (if option.doFoldVariable then foldVariable else pure)
        >=> (if option.doInlineConstructor then usingReaderT mempty . inlineConstructor else pure)
        >=> (if option.doEliminateUnusedLet then eliminateUnusedLet else pure)
        >=> (if option.doInlineFunction then flip evalStateT state . inlineFunction else pure)
        >=> (if option.doFoldRedundantCast then foldRedundantCast else pure)
        >=> (if option.doFoldTrivialCall then foldTrivialCall else pure)
        >=> (if option.doSpecializeFunction then specializeFunction else pure)
        >=> runFlat . flatExpr

-- | (let ((f (fun ps body))) (f as)) = body[as/ps]
foldTrivialCall :: (MonadIO f, MonadReader OptimizeEnv f) => Expr (Id Type) -> f (Expr (Id Type))
foldTrivialCall e@Atom {} = pure e
foldTrivialCall e@Call {} = pure e
foldTrivialCall e@CallDirect {} = pure e
foldTrivialCall e@RawCall {} = pure e
foldTrivialCall e@BinOp {} = pure e
foldTrivialCall e@Cast {} = pure e
foldTrivialCall (Match v cs) = Match <$> foldTrivialCall v <*> traverseOf (traversed . appCase) foldTrivialCall cs
foldTrivialCall (Let [LocalDef f _ (Fun ps body)] (Call (Var f') as)) | f == f' = do
  us <- asks (.uniqSupply)
  foldTrivialCall =<< alpha body AlphaEnv {uniqSupply = us, subst = HashMap.fromList $ zip ps as}
foldTrivialCall (Let ds e) = Let <$> traverseOf (traversed . expr) foldTrivialCall ds <*> foldTrivialCall e
foldTrivialCall (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) foldTrivialCall cs <*> foldTrivialCall e
foldTrivialCall (SwitchUnboxed a cs e) = SwitchUnboxed a <$> traverseOf (traversed . _2) foldTrivialCall cs <*> foldTrivialCall e
foldTrivialCall (Destruct a c xs e) = Destruct a c xs <$> foldTrivialCall e
foldTrivialCall (DestructRecord a xs e) = DestructRecord a xs <$> foldTrivialCall e
foldTrivialCall (Assign x v e) = Assign x <$> foldTrivialCall v <*> foldTrivialCall e
foldTrivialCall e@Error {} = pure e

newtype CallInlineEnv = CallInlineEnv
  { inlinableMap :: HashMap (Id Type) ([Id Type], Expr (Id Type))
  }

-- | Inline a function call.
inlineFunction ::
  (MonadState CallInlineEnv f, MonadReader OptimizeEnv f, MonadIO f) =>
  Expr (Id Type) ->
  f (Expr (Id Type))
inlineFunction e@Atom {} = pure e
inlineFunction (Call (Var f) xs) = lookupCallInline (Call . Var) f xs
inlineFunction e@Call {} = pure e
inlineFunction (CallDirect f xs) = lookupCallInline CallDirect f xs
inlineFunction e@RawCall {} = pure e
inlineFunction e@BinOp {} = pure e
inlineFunction e@Cast {} = pure e
inlineFunction (Let ds e) = do
  ds' <- traverseOf (traversed . expr) inlineFunction ds
  traverse_ checkInlinable ds'
  Let ds' <$> inlineFunction e
inlineFunction (Match v cs) =
  Match <$> inlineFunction v <*> traverseOf (traversed . appCase) inlineFunction cs
inlineFunction (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) inlineFunction cs <*> inlineFunction e
inlineFunction (SwitchUnboxed a cs e) = SwitchUnboxed a <$> traverseOf (traversed . _2) inlineFunction cs <*> inlineFunction e
inlineFunction (Destruct a c xs e) = Destruct a c xs <$> inlineFunction e
inlineFunction (DestructRecord a xs e) = DestructRecord a xs <$> inlineFunction e
inlineFunction (Assign x v e) = Assign x <$> inlineFunction v <*> inlineFunction e
inlineFunction e@Error {} = pure e

-- | Check if a function is inlinable.
checkInlinable ::
  (MonadState CallInlineEnv m, MonadReader OptimizeEnv m) =>
  LocalDef (Id Type) ->
  m ()
checkInlinable (LocalDef f _ (Fun ps v)) = do
  threshold <- asks (.option.inlineThreshold)
  -- ノードの数がthreshold以下ならインライン展開する
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
inlineConstructor e@Atom {} = pure e
inlineConstructor e@Call {} = pure e
inlineConstructor e@CallDirect {} = pure e
inlineConstructor e@RawCall {} = pure e
inlineConstructor e@BinOp {} = pure e
inlineConstructor e@Cast {} = pure e
inlineConstructor (Let ds e) = do
  ds' <- traverseOf (traversed . expr) inlineConstructor ds
  local (mconcat (map toPackInlineMap ds') <>) $ Let ds' <$> inlineConstructor e
  where
    toPackInlineMap (LocalDef v _ (Pack _ con as)) = one (v, (con, as))
    toPackInlineMap _ = mempty
inlineConstructor (Match (Atom (Var v)) [Unpack con xs body]) = do
  body' <- inlineConstructor body
  view (at v) >>= \case
    Just (con', as) | con == con' -> pure $ build xs as body'
    _ -> pure $ Destruct (Var v) con xs body'
  where
    build (x : xs) (a : as) body = Assign x (Atom a) (build xs as body)
    build _ _ body = body
inlineConstructor (Match v cs) =
  Match <$> inlineConstructor v <*> traverseOf (traversed . appCase) inlineConstructor cs
inlineConstructor (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) inlineConstructor cs <*> inlineConstructor e
inlineConstructor (SwitchUnboxed a cs e) = SwitchUnboxed a <$> traverseOf (traversed . _2) inlineConstructor cs <*> inlineConstructor e
inlineConstructor (Destruct (Var v) con xs body) = do
  body' <- inlineConstructor body
  view (at v) >>= \case
    Just (con', as) | con == con' -> pure $ build xs as body'
    _ -> pure $ Destruct (Var v) con xs body'
  where
    build (x : xs) (a : as) body = Assign x (Atom a) (build xs as body)
    build _ _ body = body
inlineConstructor (Destruct a c xs body) = Destruct a c xs <$> inlineConstructor body
inlineConstructor (DestructRecord v xs body) = DestructRecord v xs <$> inlineConstructor body
inlineConstructor (Assign x v e) = Assign x <$> inlineConstructor v <*> inlineConstructor e
inlineConstructor e@Error {} = pure e

-- | Remove variable binding if that variable is an alias of another variable.
foldVariable :: (Eq a, Applicative f) => Expr a -> f (Expr a)
foldVariable e@Atom {} = pure e
foldVariable e@Call {} = pure e
foldVariable e@CallDirect {} = pure e
foldVariable e@RawCall {} = pure e
foldVariable e@BinOp {} = pure e
foldVariable e@Cast {} = pure e
foldVariable (Let ds e) = Let <$> traverseOf (traversed . expr) foldVariable ds <*> foldVariable e
foldVariable (Match (Atom a) [Bind x _ e]) = replaceOf atom (Var x) a <$> foldVariable e
foldVariable (Match v cs) = Match <$> foldVariable v <*> traverseOf (traversed . appCase) foldVariable cs
foldVariable (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) foldVariable cs <*> foldVariable e
foldVariable (SwitchUnboxed a cs e) = SwitchUnboxed a <$> traverseOf (traversed . _2) foldVariable cs <*> foldVariable e
foldVariable (Destruct a c xs e) = Destruct a c xs <$> foldVariable e
foldVariable (DestructRecord a xs e) = DestructRecord a xs <$> foldVariable e
foldVariable (Assign x (Atom a) e) = replaceOf atom (Var x) a <$> foldVariable e
foldVariable (Assign x v e) = Assign x <$> foldVariable v <*> foldVariable e
foldVariable e@Error {} = pure e

-- | Remove unused let bindings
-- Let bindings only bind expressions that allocate memory. So we can remove unused let bindings safely.
eliminateUnusedLet :: (Monad f, Hashable a) => Expr (Id a) -> f (Expr (Id a))
eliminateUnusedLet e@Atom {} = pure e
eliminateUnusedLet e@Call {} = pure e
eliminateUnusedLet e@CallDirect {} = pure e
eliminateUnusedLet e@RawCall {} = pure e
eliminateUnusedLet e@BinOp {} = pure e
eliminateUnusedLet e@Cast {} = pure e
eliminateUnusedLet (Let ds e) = do
  ds' <- traverseOf (traversed . expr) eliminateUnusedLet ds
  e' <- eliminateUnusedLet e
  -- 定義vから到達可能でかつvで定義されていない変数すべての集合のマップ
  let gamma = map (\(LocalDef v _ o) -> (v, HashSet.delete v $ freevars o)) ds'
  if any (\(LocalDef v _ _) -> reachable 100 gamma v $ freevars e') ds' then pure $ Let ds' e' else pure e'
  where
    reachable :: Hashable a => Int -> [(Id a, HashSet (Id a))] -> Id a -> HashSet (Id a) -> Bool
    reachable limit gamma v fvs
      -- limit回試行してわからなければ安全側に倒してTrue
      | limit <= 0 = True
      | idIsExternal v = True
      | v `member` fvs = True
      | otherwise =
          -- fvsの要素fvについて、gamma[fv]をfvsに加える
          -- fvsに変化がなければ、vはどこからも参照されていない
          let fvs' = fvs <> mconcat (mapMaybe (List.lookup ?? gamma) $ HashSet.toList fvs)
           in fvs /= fvs' && reachable limit gamma v fvs'
eliminateUnusedLet (Match v cs) =
  Match <$> eliminateUnusedLet v <*> traverseOf (traversed . appCase) eliminateUnusedLet cs
eliminateUnusedLet (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) eliminateUnusedLet cs <*> eliminateUnusedLet e
eliminateUnusedLet (SwitchUnboxed a cs e) = SwitchUnboxed a <$> traverseOf (traversed . _2) eliminateUnusedLet cs <*> eliminateUnusedLet e
eliminateUnusedLet (Destruct a c xs e) = Destruct a c xs <$> eliminateUnusedLet e
eliminateUnusedLet (DestructRecord a xs e) = DestructRecord a xs <$> eliminateUnusedLet e
eliminateUnusedLet (Assign x v e) = Assign x <$> eliminateUnusedLet v <*> eliminateUnusedLet e
eliminateUnusedLet e@Error {} = pure e

-- | Remove a cast if it is redundant.
foldRedundantCast :: (HasType a, Applicative f) => Expr a -> f (Expr a)
foldRedundantCast e@Atom {} = pure e
foldRedundantCast e@Call {} = pure e
foldRedundantCast e@CallDirect {} = pure e
foldRedundantCast e@RawCall {} = pure e
foldRedundantCast e@BinOp {} = pure e
foldRedundantCast (Cast t e)
  | typeOf e == t = pure (Atom e)
  | otherwise = pure (Cast t e)
foldRedundantCast (Let ds e) = Let <$> traverseOf (traversed . expr) foldRedundantCast ds <*> foldRedundantCast e
foldRedundantCast (Match v cs) = Match <$> foldRedundantCast v <*> traverseOf (traversed . appCase) foldRedundantCast cs
foldRedundantCast (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) foldRedundantCast cs <*> foldRedundantCast e
foldRedundantCast (SwitchUnboxed a cs e) = SwitchUnboxed a <$> traverseOf (traversed . _2) foldRedundantCast cs <*> foldRedundantCast e
foldRedundantCast (Destruct a c xs e) = Destruct a c xs <$> foldRedundantCast e
foldRedundantCast (DestructRecord a xs e) = DestructRecord a xs <$> foldRedundantCast e
foldRedundantCast (Assign x v e) = Assign x <$> foldRedundantCast v <*> foldRedundantCast e
foldRedundantCast e@Error {} = pure e

-- | Specialize a function which is casted to a specific type.
-- TODO: ベンチマーク
specializeFunction :: (MonadIO m, MonadReader OptimizeEnv m) => Expr (Id Type) -> m (Expr (Id Type))
specializeFunction e@Atom {} = pure e
specializeFunction e@Call {} = pure e
specializeFunction e@CallDirect {} = pure e
specializeFunction e@RawCall {} = pure e
specializeFunction e@BinOp {} = pure e
specializeFunction e@(Cast (pts' :-> rt') f) = case typeOf f of
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
  _ -> pure e
specializeFunction e@Cast {} = pure e
specializeFunction (Match v cs) = Match <$> specializeFunction v <*> traverseOf (traversed . appCase) specializeFunction cs
specializeFunction (Let ds e) = Let <$> traverseOf (traversed . expr) specializeFunction ds <*> specializeFunction e
specializeFunction (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) specializeFunction cs <*> specializeFunction e
specializeFunction (SwitchUnboxed a cs e) = SwitchUnboxed a <$> traverseOf (traversed . _2) specializeFunction cs <*> specializeFunction e
specializeFunction (Destruct a c xs e) = Destruct a c xs <$> specializeFunction e
specializeFunction (DestructRecord a xs e) = DestructRecord a xs <$> specializeFunction e
specializeFunction (Assign x v e) = Assign x <$> specializeFunction v <*> specializeFunction e
specializeFunction e@Error {} = pure e