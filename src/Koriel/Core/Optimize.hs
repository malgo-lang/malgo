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

data OptimizeEnv = OptimizeEnv {uniqSupply :: UniqSupply, moduleName :: ModuleName, inlineLevel :: Int}

makeFieldsNoPrefix ''OptimizeEnv

-- | Apply a monadic function n times.
times :: Monad m => Int -> (t -> m t) -> t -> m t
times 0 _ x = pure x
times n f x
  | n > 0 = times (n - 1) f =<< f x
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
  -- | インライン展開する関数のサイズ
  Int ->
  Program (Id Type) ->
  m (Program (Id Type))
optimizeProgram us moduleName level Program {..} = runReaderT ?? OptimizeEnv {uniqSupply = us, moduleName, inlineLevel = level} $ do
  state <- execStateT ?? CallInlineEnv mempty $ for_ topFuns $ \(name, ps, t, e) -> checkInlinable $ LocalDef name t (Fun ps e)
  topVars <- traverse (\(n, t, e) -> (n,t,) <$> optimizeExpr state e) topVars
  topFuns <- traverse (\(n, ps, t, e) -> (n,ps,t,) <$> optimizeExpr (CallInlineEnv $ HashMap.delete n state.inlinableMap) e) topFuns
  pure $ Program {..}

optimizeExpr :: (MonadReader OptimizeEnv f, MonadIO f) => CallInlineEnv -> Exp (Id Type) -> f (Exp (Id Type))
optimizeExpr state = 5 `times` opt
  where
    opt = do
      pure
        >=> optVarBind
        >=> (usingReaderT mempty . optPackInline)
        >=> removeUnusedLet
        >=> (flip evalStateT state . optCallInline)
        >=> optIdCast
        >=> optTrivialCall
        >=> runFlat . flatExp

-- (let ((f (fun ps body))) (f as)) = body[as/ps]
optTrivialCall :: (MonadIO f, MonadReader OptimizeEnv f) => Exp (Id Type) -> f (Exp (Id Type))
optTrivialCall (Let [LocalDef f _ (Fun ps body)] (Call (Var f') as)) | f == f' = do
  us <- asks (.uniqSupply)
  optTrivialCall =<< alpha body AlphaEnv {uniqSupply = us, subst = HashMap.fromList $ zip ps as}
optTrivialCall (Let ds e) = Let <$> traverseOf (traversed . object . appObj) optTrivialCall ds <*> optTrivialCall e
optTrivialCall (Match v cs) = Match <$> optTrivialCall v <*> traverseOf (traversed . appCase) optTrivialCall cs
optTrivialCall (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) optTrivialCall cs <*> optTrivialCall e
optTrivialCall (Destruct a c xs e) = Destruct a c xs <$> optTrivialCall e
optTrivialCall (Assign x v e) = Assign x <$> optTrivialCall v <*> optTrivialCall e
optTrivialCall e = pure e

newtype CallInlineEnv = CallInlineEnv
  { inlinableMap :: HashMap (Id Type) ([Id Type], Exp (Id Type))
  }

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
optCallInline (Assign x v e) = Assign x <$> optCallInline v <*> optCallInline e
optCallInline (Let ds e) = do
  ds' <- traverseOf (traversed . object . appObj) optCallInline ds
  traverse_ checkInlinable ds'
  Let ds' <$> optCallInline e
optCallInline e = pure e

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

lookupCallInline ::
  (MonadReader OptimizeEnv m, MonadState CallInlineEnv m, MonadIO m) =>
  (Id Type -> [Atom (Id Type)] -> Exp (Id Type)) ->
  Id Type ->
  [Atom (Id Type)] ->
  m (Exp (Id Type))
lookupCallInline call f as = do
  f' <- gets $ (.inlinableMap) >>> HashMap.lookup f
  us <- asks (.uniqSupply)
  case f' of
    Just (ps, v) -> alpha v AlphaEnv {uniqSupply = us, subst = HashMap.fromList $ zip ps as}
    Nothing -> pure $ call f as

type PackInlineMap = HashMap (Id Type) (Con, [Atom (Id Type)])

optPackInline :: MonadReader PackInlineMap m => Exp (Id Type) -> m (Exp (Id Type))
optPackInline (Match (Atom (Var v)) [Unpack con xs body]) = do
  body' <- optPackInline body
  view (at v) >>= \case
    Just (con', as) | con == con' -> pure $ build xs as body'
    _ -> pure $ Match (Atom $ Var v) [Unpack con xs body']
  where
    build (x : xs) (a : as) body = Match (Atom a) [Bind x (typeOf x) (build xs as body)]
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
    build (x : xs) (a : as) body = Match (Atom a) [Bind x (typeOf x) (build xs as body)]
    build _ _ body = body
optPackInline (Assign x v e) = Assign x <$> optPackInline v <*> optPackInline e
optPackInline (Let ds e) = do
  ds' <- traverseOf (traversed . object . appObj) optPackInline ds
  local (mconcat (map toPackInlineMap ds') <>) $ Let ds' <$> optPackInline e
  where
    toPackInlineMap (LocalDef v _ (Pack _ con as)) = one (v, (con, as))
    toPackInlineMap _ = mempty
optPackInline e = pure e

optVarBind :: (Eq a, Applicative f) => Exp a -> f (Exp a)
optVarBind (Match (Atom a) [Bind x _ e]) = replaceOf atom (Var x) a <$> optVarBind e
optVarBind (Let ds e) = Let <$> traverseOf (traversed . object . appObj) optVarBind ds <*> optVarBind e
optVarBind (Match v cs) = Match <$> optVarBind v <*> traverseOf (traversed . appCase) optVarBind cs
optVarBind (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) optVarBind cs <*> optVarBind e
optVarBind (Destruct a c xs e) = Destruct a c xs <$> optVarBind e
optVarBind (Assign x (Atom a) e) = replaceOf atom (Var x) a <$> optVarBind e
optVarBind e = pure e

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
removeUnusedLet (Assign x v e) = Assign x <$> removeUnusedLet v <*> removeUnusedLet e
removeUnusedLet e = pure e

optIdCast :: (HasType a, Applicative f) => Exp a -> f (Exp a)
optIdCast (Cast t e) | typeOf e == t = pure (Atom e)
optIdCast (Let ds e) = Let <$> traverseOf (traversed . object . appObj) optIdCast ds <*> optIdCast e
optIdCast (Match v cs) = Match <$> optIdCast v <*> traverseOf (traversed . appCase) optIdCast cs
optIdCast (Switch a cs e) = Switch a <$> traverseOf (traversed . _2) optIdCast cs <*> optIdCast e
optIdCast (Destruct a c xs e) = Destruct a c xs <$> optIdCast e
optIdCast (Assign x v e) = Assign x <$> optIdCast v <*> optIdCast e
optIdCast e = pure e
