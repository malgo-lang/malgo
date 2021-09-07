module Koriel.Core.Optimize
  ( optimizeProgram,
  )
where

import Control.Lens (At (at), Lens', cosmos, lengthOf, lens, over, traverseOf, traversed, view, (?=), (?~), _2)
import Control.Monad.Except
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Koriel.Core.Alpha
import Koriel.Core.Flat
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Relude.Extra.Map (StaticMap (member))

data OptimizeEnv = OptimizeEnv {_optimizeUniqSupply :: UniqSupply, _inlineLevel :: Int}

optimizeUniqSupply :: Lens' OptimizeEnv UniqSupply
optimizeUniqSupply = lens _optimizeUniqSupply (\o x -> o {_optimizeUniqSupply = x})

inlineLevel :: Lens' OptimizeEnv Int
inlineLevel = lens _inlineLevel (\o x -> o {_inlineLevel = x})

instance HasUniqSupply OptimizeEnv where
  uniqSupply = optimizeUniqSupply

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
  -- | インライン展開する関数のサイズ
  Int ->
  Program (Id Type) ->
  m (Program (Id Type))
optimizeProgram us level prog@Program {..} = runReaderT ?? OptimizeEnv {_optimizeUniqSupply = us, _inlineLevel = level} $ do
  state <- execStateT (traverse (checkInlineable . uncurry LocalDef . over _2 (uncurry Fun)) _topFuncs) mempty
  appProgram (optimizeExpr state) prog

optimizeExpr :: (MonadReader OptimizeEnv f, MonadIO f) => CallInlineMap -> Exp (Id Type) -> f (Exp (Id Type))
optimizeExpr state = 10 `times` opt
  where
    opt =
      pure
        >=> optVarBind
        >=> (usingReaderT mempty . optPackInline)
        >=> removeUnusedLet
        >=> (flip evalStateT state . optCallInline)
        >=> optIdCast
        >=> optTrivialCall
        -- TODO[optCast]
        -- >=> optCast
        >=> pure
          . flat

-- (let ((f (fun () body))) (f)) = body
optTrivialCall :: (Eq a, Applicative f) => Exp a -> f (Exp a)
optTrivialCall (Let [LocalDef f (Fun [] body)] (Call (Var f') [])) | f == f' = optTrivialCall body
optTrivialCall (Let ds e) = Let <$> traverseOf (traversed . localDefObj . appObj) optTrivialCall ds <*> optTrivialCall e
optTrivialCall (Match v cs) = Match <$> optTrivialCall v <*> traverseOf (traversed . appCase) optTrivialCall cs
optTrivialCall e = pure e

type CallInlineMap = HashMap (Id Type) ([Id Type], Exp (Id Type))

optCallInline ::
  (MonadState CallInlineMap f, MonadReader OptimizeEnv f, MonadIO f) =>
  Exp (Id Type) ->
  f (Exp (Id Type))
optCallInline (Call (Var f) xs) = lookupCallInline (Call . Var) f xs
optCallInline (CallDirect f xs) = lookupCallInline CallDirect f xs
optCallInline (Match v cs) =
  Match <$> optCallInline v <*> traverseOf (traversed . appCase) optCallInline cs
optCallInline (Let ds e) = do
  ds' <- traverseOf (traversed . localDefObj . appObj) optCallInline ds
  traverse_ checkInlineable ds'
  Let ds' <$> optCallInline e
optCallInline e = pure e

checkInlineable ::
  (MonadState CallInlineMap m, MonadReader OptimizeEnv m) =>
  LocalDef (Id Type) ->
  m ()
checkInlineable (LocalDef f (Fun ps v)) = do
  level <- view inlineLevel
  -- ノードの数がlevel以下ならインライン展開する
  when (lengthOf cosmos v <= level {-  || f `notElem` freevars v -}) $ at f ?= (ps, v)
checkInlineable _ = pass

lookupCallInline ::
  (MonadReader OptimizeEnv m, MonadState CallInlineMap m, MonadIO m) =>
  (Id Type -> [Atom (Id Type)] -> Exp (Id Type)) ->
  Id Type ->
  [Atom (Id Type)] ->
  m (Exp (Id Type))
lookupCallInline call f as = do
  f' <- gets (view (at f))
  us <- view uniqSupply
  case f' of
    Just (ps, v) -> alpha v AlphaEnv {_alphaUniqSupply = us, _alphaMap = HashMap.fromList $ zip ps as}
    Nothing -> pure $ call f as

type PackInlineMap = HashMap (Id Type) (Con, [Atom (Id Type)])

optPackInline :: MonadReader PackInlineMap m => Exp (Id Type) -> m (Exp (Id Type))
optPackInline (Match (Atom (Var v)) (Unpack con xs body :| [])) = do
  body' <- optPackInline body
  mPack <- view (at v)
  case mPack of
    Just (con', as) | con == con' -> pure $ build xs as body'
    _ -> pure $ Match (Atom $ Var v) $ Unpack con xs body' :| []
  where
    build (x : xs) (a : as) body = Match (Atom a) $ Bind x (build xs as body) :| []
    build _ _ body = body
optPackInline (Match v cs) =
  Match <$> optPackInline v <*> traverseOf (traversed . appCase) optPackInline cs
optPackInline (Let ds e) = do
  ds' <- traverseOf (traversed . localDefObj . appObj) optPackInline ds
  local (mconcat (map toPackInlineMap ds') <>) $ Let ds' <$> optPackInline e
  where
    toPackInlineMap (LocalDef v (Pack _ con as)) = mempty & at v ?~ (con, as)
    toPackInlineMap _ = mempty
optPackInline e = pure e

optVarBind :: (Eq a, Applicative f) => Exp a -> f (Exp a)
optVarBind (Match (Atom a) (Bind x e :| [])) = replaceOf atom (Var x) a <$> optVarBind e
optVarBind (Let ds e) = Let <$> traverseOf (traversed . localDefObj . appObj) optVarBind ds <*> optVarBind e
optVarBind (Match v cs) = Match <$> optVarBind v <*> traverseOf (traversed . appCase) optVarBind cs
optVarBind e = pure e

removeUnusedLet :: (Monad f, Eq a) => Exp (Id a) -> f (Exp (Id a))
removeUnusedLet (Let ds e) = do
  ds' <- traverseOf (traversed . localDefObj . appObj) removeUnusedLet ds
  e' <- removeUnusedLet e
  -- 定義vから到達可能でかつvで定義されていない変数すべての集合のマップ
  let gamma = map (\(LocalDef v o) -> (v, HashSet.delete v $ freevars o)) ds'
  if any (\(LocalDef v _) -> reachable 100 gamma v $ freevars e') ds' then pure $ Let ds' e' else pure e'
  where
    reachable :: Eq a => Int -> [(Id a, HashSet (Id a))] -> Id a -> HashSet (Id a) -> Bool
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
removeUnusedLet e = pure e

optIdCast :: (HasType a, Applicative f) => Exp a -> f (Exp a)
optIdCast (Cast t e) | typeOf e == t = pure (Atom e)
optIdCast (Let ds e) = Let <$> traverseOf (traversed . localDefObj . appObj) optIdCast ds <*> optIdCast e
optIdCast (Match v cs) = Match <$> optIdCast v <*> traverseOf (traversed . appCase) optIdCast cs
optIdCast e = pure e

-- 効果がはっきりしないので一旦コメントアウト
-- TODO[optCast] ベンチマーク
-- optCast :: MonadUniq f => Exp (Id Type) -> f (Exp (Id Type))
-- optCast e@(Cast (pts' :-> rt') f) = case typeOf f of
--   pts :-> _
--     | length pts' == length pts -> do
--       f' <- newInternalId "$cast_opt" (pts' :-> rt')
--       ps' <- traverse (newInternalId "$p") pts'
--       v' <- runDef do
--         ps <- zipWithM cast pts $ map (Atom . Var) ps'
--         r <- bind (Call f ps)
--         pure $ Cast rt' r
--       pure (Let [LocalDef f' $ Fun ps' v'] (Atom $ Var f'))
--     | otherwise -> bug Unreachable
--   _ -> pure e
-- optCast (Match v cs) = Match <$> optCast v <*> traverseOf (traversed . appCase) optCast cs
-- optCast (Let ds e) = Let <$> traverseOf (traversed . localDefObj . appObj) optCast ds <*> optCast e
-- optCast e = pure e
