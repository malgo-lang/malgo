{-# LANGUAGE TemplateHaskell #-}

module Koriel.Core.Optimize
  ( optimizeProgram,
  )
where

import Control.Lens (At (at), makeFieldsNoPrefix, traverseOf, traversed, view, _2)
import Control.Monad.Except
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

data OptimizeEnv = OptimizeEnv {uniqSupply :: UniqSupply, moduleName :: ModuleName, inlineLevel :: Int, knowns :: HashSet (Id Type)}

makeFieldsNoPrefix ''OptimizeEnv

-- | Apply a monadic function n times.
times :: (Monad m, Eq t) => Int -> (t -> m t) -> t -> m t
times 0 _ x = pure x
times n f x
  | n > 0 = do
      x' <- f x
      if x == x'
        then pure x
        else times (n - 1) f x'
  | otherwise = error $ show n <> " must be a natural number"

-- | 最適化を行う関数
--
-- * 自明な変数の付け替えの簡約 (optVarBind)
-- * 値コンストラクタを含む、関数のインライン展開 (optPackInline, optCallInline)
-- * 不要なletの削除 (removeUnusedLet)
-- * 無意味なcastの削除（optIdCast）
optimizeProgram ::
  MonadIO m =>
  UniqSupply ->
  ModuleName ->
  HashSet (Id Type) ->
  -- | インライン展開する関数のサイズ
  Int ->
  Program (Id Type) ->
  m (Program (Id Type))
optimizeProgram uniqSupply moduleName knowns inlineLevel Program {..} = runReaderT ?? OptimizeEnv {..} $ do
  state <- execStateT ?? CallInlineEnv mempty $ for_ topFuns $ \(name, ps, t, e) -> checkInlinable $ LocalDef name t (Fun ps e)
  topVars <- traverse (\(n, t, e) -> (n,t,) <$> optimizeExpr state e) topVars
  topFuns <- traverse (\(n, ps, t, e) -> (n,ps,t,) <$> optimizeExpr (CallInlineEnv $ HashMap.delete n state.inlinableMap) e) topFuns
  pure $ Program {..}

optimizeExpr :: (MonadReader OptimizeEnv f, MonadIO f) => CallInlineEnv -> Exp (Id Type) -> f (Exp (Id Type))
optimizeExpr state = 10 `times` opt
  where
    opt = do
      pure
        >=> optVarBind
        >=> (usingReaderT mempty . optPackInline)
        >=> removeUnusedLet
        >=> (flip evalStateT state . optCallInline)
        >=> optIdCast
        >=> optTrivialCall
        >=> optCast
        >=> runFlat . flatExp

-- | (let ((f (fun ps body))) (f as)) = body[as/ps]
optTrivialCall :: (MonadIO f, MonadReader OptimizeEnv f) => Exp (Id Type) -> f (Exp (Id Type))
optTrivialCall (Let [LocalDef f _ (Fun ps body)] (Call (Var f') as)) | f == f' = do
  us <- asks (.uniqSupply)
  optTrivialCall =<< alpha body AlphaEnv {uniqSupply = us, subst = HashMap.fromList $ zip ps as}
optTrivialCall (Let ds e) = Let <$> traverseOf (traversed . object . appObj) optTrivialCall ds <*> optTrivialCall e
optTrivialCall (Match v cs) = Match <$> optTrivialCall v <*> traverseOf (traversed . appCase) optTrivialCall cs
optTrivialCall (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) optTrivialCall cs <*> optTrivialCall e
optTrivialCall (Destruct a c xs e) = Destruct a c xs <$> optTrivialCall e
optTrivialCall (DestructRecord a xs e) = DestructRecord a xs <$> optTrivialCall e
optTrivialCall (Assign x v e) = Assign x <$> optTrivialCall v <*> optTrivialCall e
optTrivialCall e = pure e

newtype CallInlineEnv = CallInlineEnv
  { inlinableMap :: HashMap (Id Type) ([Id Type], Exp (Id Type))
  }

-- | Inline a function call.
optCallInline ::
  (MonadState CallInlineEnv f, MonadReader OptimizeEnv f, MonadIO f) =>
  Exp (Id Type) ->
  f (Exp (Id Type))
optCallInline (Call (Var f) xs) = lookupCallInline (Call . Var) f xs
optCallInline (CallDirect f xs) = lookupCallInline CallDirect f xs
optCallInline (Match v cs) =
  Match <$> optCallInline v <*> traverseOf (traversed . appCase) optCallInline cs
optCallInline (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) optCallInline cs <*> optCallInline e
optCallInline (Destruct a c xs e) = Destruct a c xs <$> optCallInline e
optCallInline (DestructRecord a xs e) = DestructRecord a xs <$> optCallInline e
optCallInline (Assign x v e) = Assign x <$> optCallInline v <*> optCallInline e
optCallInline (Let ds e) = do
  ds' <- traverseOf (traversed . object . appObj) optCallInline ds
  traverse_ checkInlinable ds'
  Let ds' <$> optCallInline e
optCallInline e = pure e

-- | Check if a function is inlinable.
checkInlinable ::
  (MonadState CallInlineEnv m, MonadReader OptimizeEnv m) =>
  LocalDef (Id Type) ->
  m ()
checkInlinable (LocalDef f _ (Fun ps v)) = do
  level <- asks (.inlineLevel)
  -- 変数の数がlevel以下ならインライン展開する
  let isInlinableSize = level >= length v
  when isInlinableSize $ do
    modify $ \e -> e {inlinableMap = HashMap.insert f (ps, v) e.inlinableMap}
checkInlinable _ = pass

-- | Lookup a function in the inlinable map.
lookupCallInline ::
  (MonadReader OptimizeEnv m, MonadState CallInlineEnv m) =>
  (Id Type -> [Atom (Id Type)] -> Exp (Id Type)) ->
  Id Type ->
  [Atom (Id Type)] ->
  m (Exp (Id Type))
lookupCallInline call f as = do
  f' <- gets $ (.inlinableMap) >>> HashMap.lookup f
  pure case f' of
    Just (ps, v) -> foldl' (\e (x, a) -> Assign x (Atom a) e) v $ zip ps as
    Nothing -> call f as

type PackInlineMap = HashMap (Id Type) (Con, [Atom (Id Type)])

-- | Inline simple pattern match and pack.
optPackInline :: MonadReader PackInlineMap m => Exp (Id Type) -> m (Exp (Id Type))
optPackInline (Match (Atom (Var v)) [Unpack con xs body]) = do
  body' <- optPackInline body
  view (at v) >>= \case
    Just (con', as) | con == con' -> pure $ build xs as body'
    _ -> pure $ Destruct (Var v) con xs body'
  where
    build (x : xs) (a : as) body = Assign x (Atom a) (build xs as body)
    build _ _ body = body
optPackInline (Match v cs) =
  Match <$> optPackInline v <*> traverseOf (traversed . appCase) optPackInline cs
optPackInline (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) optPackInline cs <*> optPackInline e
optPackInline (Destruct (Var v) con xs body) = do
  body' <- optPackInline body
  view (at v) >>= \case
    Just (con', as) | con == con' -> pure $ build xs as body'
    _ -> pure $ Destruct (Var v) con xs body'
  where
    build (x : xs) (a : as) body = Assign x (Atom a) (build xs as body)
    build _ _ body = body
optPackInline (DestructRecord v xs body) = DestructRecord v xs <$> optPackInline body
optPackInline (Assign x v e) = Assign x <$> optPackInline v <*> optPackInline e
optPackInline (Let ds e) = do
  ds' <- traverseOf (traversed . object . appObj) optPackInline ds
  local (mconcat (map toPackInlineMap ds') <>) $ Let ds' <$> optPackInline e
  where
    toPackInlineMap (LocalDef v _ (Pack _ con as)) = one (v, (con, as))
    toPackInlineMap _ = mempty
optPackInline e = pure e

-- | Remove variable binding if that variable is an alias of another variable.
optVarBind :: (Eq a, Applicative f) => Exp a -> f (Exp a)
optVarBind (Match (Atom a) [Bind x _ e]) = replaceOf atom (Var x) a <$> optVarBind e
optVarBind (Let ds e) = Let <$> traverseOf (traversed . object . appObj) optVarBind ds <*> optVarBind e
optVarBind (Match v cs) = Match <$> optVarBind v <*> traverseOf (traversed . appCase) optVarBind cs
optVarBind (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) optVarBind cs <*> optVarBind e
optVarBind (Destruct a c xs e) = Destruct a c xs <$> optVarBind e
optVarBind (DestructRecord a xs e) = DestructRecord a xs <$> optVarBind e
optVarBind (Assign x (Atom a) e) = replaceOf atom (Var x) a <$> optVarBind e
optVarBind e = pure e

-- | Remove unused let bindings
-- Let bindings only bind expressions that allocate memory. So we can remove unused let bindings safely.
removeUnusedLet :: (Monad f, Hashable a) => Exp (Id a) -> f (Exp (Id a))
removeUnusedLet (Let ds e) = do
  ds' <- traverseOf (traversed . object . appObj) removeUnusedLet ds
  e' <- removeUnusedLet e
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
removeUnusedLet (Match v cs) =
  Match <$> removeUnusedLet v <*> traverseOf (traversed . appCase) removeUnusedLet cs
removeUnusedLet (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) removeUnusedLet cs <*> removeUnusedLet e
removeUnusedLet (Destruct a c xs e) = Destruct a c xs <$> removeUnusedLet e
removeUnusedLet (DestructRecord a xs e) = DestructRecord a xs <$> removeUnusedLet e
removeUnusedLet (Assign x v e) = Assign x <$> removeUnusedLet v <*> removeUnusedLet e
removeUnusedLet e = pure e

-- | Remove a cast if it is redundant.
optIdCast :: (HasType a, Applicative f) => Exp a -> f (Exp a)
optIdCast (Cast t e) | typeOf e == t = pure (Atom e)
optIdCast (Let ds e) = Let <$> traverseOf (traversed . object . appObj) optIdCast ds <*> optIdCast e
optIdCast (Match v cs) = Match <$> optIdCast v <*> traverseOf (traversed . appCase) optIdCast cs
optIdCast (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) optIdCast cs <*> optIdCast e
optIdCast (Destruct a c xs e) = Destruct a c xs <$> optIdCast e
optIdCast (DestructRecord a xs e) = DestructRecord a xs <$> optIdCast e
optIdCast (Assign x v e) = Assign x <$> optIdCast v <*> optIdCast e
optIdCast e = pure e

-- TODO: ベンチマーク

-- | Specialize a function which is casted to a specific type.
optCast :: (MonadIO m, MonadReader OptimizeEnv m) => Exp (Id Type) -> m (Exp (Id Type))
optCast e@(Cast (pts' :-> rt') f) = case typeOf f of
  pts :-> _
    | length pts' == length pts -> do
        f' <- newInternalId "cast_opt" (pts' :-> rt')
        ps' <- traverse (newInternalId "p") pts'
        v' <- runDef do
          ps <- zipWithM cast pts $ map (Atom . Var) ps'

          ks <- asks (.knowns)

          r <- case f of
            -- If `f` is a known function, we must call it directly.
            Var f | f `member` ks -> do
              bind (CallDirect f ps)
            _ -> bind (Call f ps)
          pure $ Cast rt' r
        pure (Let [LocalDef f' (pts' :-> rt') $ Fun ps' v'] (Atom $ Var f'))
    | otherwise -> error "optCast: invalid cast"
  _ -> pure e
optCast (Match v cs) = Match <$> optCast v <*> traverseOf (traversed . appCase) optCast cs
optCast (Let ds e) = Let <$> traverseOf (traversed . object . appObj) optCast ds <*> optCast e
optCast (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) optCast cs <*> optCast e
optCast (Destruct a c xs e) = Destruct a c xs <$> optCast e
optCast (DestructRecord a xs e) = DestructRecord a xs <$> optCast e
optCast (Assign x v e) = Assign x <$> optCast v <*> optCast e
optCast e = pure e