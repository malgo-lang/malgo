-- | Alpha conversion
module Koriel.Core.Alpha
  ( alpha,
    equiv,
  )
where

import Control.Exception (assert)
import Control.Lens (traverseOf)
import Data.HashMap.Strict qualified as HashMap
import GHC.Records (HasField)
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude

type Subst = HashMap (Id Type) (Atom (Id Type))

-- | Alpha conversion
alpha ::
  ( MonadIO m,
    HasField "moduleName" r ModuleName,
    HasField "uniqSupply" r UniqSupply,
    MonadReader r m
  ) =>
  Expr (Id Type) ->
  Subst ->
  m (Expr (Id Type))
alpha x = runAlpha $ alphaExpr x

runAlpha :: ReaderT Subst m a -> Subst -> m a
runAlpha = runReaderT

lookupVar :: (MonadReader Subst m) => Id Type -> m (Atom (Id Type))
lookupVar n = do
  HashMap.lookupDefault (Var n) n <$> ask

lookupId :: (MonadReader Subst m) => Id Type -> m (Id Type)
lookupId n = do
  n' <- lookupVar n
  case n' of
    Var i -> pure i
    _ -> error $ show n <> " must be bound to Var"

type AlphaM r t m =
  ( MonadIO m,
    MonadIO (t m),
    MonadTrans t,
    HasField "moduleName" r ModuleName,
    HasField "uniqSupply" r UniqSupply,
    MonadReader r m,
    MonadReader (HashMap (Id Type) (Atom (Id Type))) (t m)
  )

cloneId ::
  ( MonadIO m,
    MonadTrans t,
    MonadReader r m,
    HasField "moduleName" r ModuleName,
    HasField "uniqSupply" r UniqSupply
  ) =>
  Id a ->
  t m (Id a)
cloneId Id {..} = lift $ do
  assert (sort == Internal || sort == Temporal) pass -- only Internal or Temporal id can be cloned
  moduleName <- asks (.moduleName)
  uniq <- getUniq
  pure Id {..}
{-# INLINE cloneId #-}

-- alphaExpr :: (MonadReader Subst f, MonadIO f) => Expr (Id Type) -> f (Expr (Id Type))
alphaExpr ::
  (AlphaM r t m) =>
  Expr (Id Type) ->
  t m (Expr (Id Type))
alphaExpr (Atom x) = Atom <$> alphaAtom x
alphaExpr (Call f xs) = Call <$> alphaAtom f <*> traverse alphaAtom xs
alphaExpr (CallDirect f xs) = CallDirect <$> lookupId f <*> traverse alphaAtom xs
alphaExpr (RawCall n t xs) = RawCall n t <$> traverse alphaAtom xs
alphaExpr (Cast t x) = Cast t <$> alphaAtom x
alphaExpr (Let ds e) = do
  -- Avoid capturing variables
  env <- foldMapM (\(LocalDef n _ _) -> HashMap.singleton n . Var <$> cloneId n) ds
  local (env <>) $ Let <$> traverse alphaLocalDef ds <*> alphaExpr e
alphaExpr (Match e cs) = Match <$> alphaExpr e <*> traverse alphaCase cs
alphaExpr (Switch v cs e) = Switch <$> alphaAtom v <*> traverse (\(tag, e) -> (tag,) <$> alphaExpr e) cs <*> alphaExpr e
alphaExpr (SwitchUnboxed v cs e) = SwitchUnboxed <$> alphaAtom v <*> traverse (\(tag, e) -> (tag,) <$> alphaExpr e) cs <*> alphaExpr e
alphaExpr (Destruct v con xs e) = do
  -- Avoid capturing variables
  env <- foldMapM (\x -> HashMap.singleton x . Var <$> cloneId x) xs
  local (env <>) $ Destruct <$> alphaAtom v <*> pure con <*> traverse lookupId xs <*> alphaExpr e
alphaExpr (DestructRecord v kvs e) = do
  -- Avoid capturing variables
  let xs = HashMap.elems kvs
  env <- foldMapM (\x -> HashMap.singleton x . Var <$> cloneId x) xs
  local (env <>) $ DestructRecord <$> alphaAtom v <*> traverse lookupId kvs <*> alphaExpr e
alphaExpr (Assign x v e) = do
  v' <- alphaExpr v
  x' <- cloneId x
  local (HashMap.insert x (Var x')) $ Assign x' v' <$> alphaExpr e
alphaExpr (Error t) = pure $ Error t

alphaAtom :: (MonadReader Subst f) => Atom (Id Type) -> f (Atom (Id Type))
alphaAtom (Var x) = lookupVar x
alphaAtom a@Unboxed {} = pure a

alphaLocalDef :: (AlphaM r t m) => LocalDef (Id Type) -> t m (LocalDef (Id Type))
alphaLocalDef (LocalDef x t o) =
  -- Since `alphaExpr Let{}` avoids capturing variables, only `lookupId` should be applied here.
  LocalDef <$> lookupId x <*> pure t <*> alphaObj o

alphaObj ::
  (AlphaM r t m) =>
  Obj (Id Type) ->
  t m (Obj (Id Type))
alphaObj (Fun ps e) = do
  -- Avoid capturing variables
  ps' <- traverse cloneId ps
  local (HashMap.fromList (zip ps $ map Var ps') <>) $ Fun ps' <$> alphaExpr e
-- local (\e -> e {subst = HashMap.filterWithKey (\k _ -> k `notElem` ps) e.subst}) $ Fun ps <$> alphaExpr e
alphaObj o = traverseOf atom alphaAtom o

alphaCase ::
  ( MonadTrans t,
    MonadIO m,
    MonadIO (t m),
    HasField "moduleName" r ModuleName,
    HasField "uniqSupply" r UniqSupply,
    MonadReader r m,
    MonadReader (HashMap (Id Type) (Atom (Id Type))) (t m)
  ) =>
  Case (Id Type) ->
  t m (Case (Id Type))
alphaCase (Unpack c ps e) = do
  -- Avoid capturing variables
  ps' <- traverse cloneId ps
  local (HashMap.fromList (zip ps $ map Var ps') <>) $ Unpack c ps' <$> alphaExpr e
-- local (\e -> e {subst = HashMap.filterWithKey (\k _ -> k `notElem` ps) e.subst}) $ Unpack c ps <$> alphaExpr e
alphaCase (OpenRecord kps e) = do
  -- Avoid capturing variables
  let ps = HashMap.elems kps
  kps' <- traverse cloneId kps
  let ps' = HashMap.elems kps'
  local (HashMap.fromList (zip ps $ map Var ps') <>) $ OpenRecord kps' <$> alphaExpr e
-- local (\e -> e {subst = HashMap.filterWithKey (\k _ -> k `notElem` ps) e.subst}) $ OpenRecord kps <$> alphaExpr e
alphaCase (Bind x t e) = do
  -- Avoid capturing variables
  x' <- cloneId x
  local (HashMap.insert x (Var x')) $ Bind x' t <$> alphaExpr e
-- local (\e -> e {subst = HashMap.delete x e.subst}) $ Bind x <$> alphaExpr e
alphaCase (Exact u e) = Exact u <$> alphaExpr e

equiv :: (MonadIO m) => Expr (Id Type) -> Expr (Id Type) -> m (Maybe Subst)
equiv e1 e2 = runReaderT ?? mempty $ do
  isEquiv <- equivExpr e1 e2
  if isEquiv then Just <$> ask else pure Nothing

equivExpr :: (MonadReader Subst m) => Expr (Id Type) -> Expr (Id Type) -> m Bool
equivExpr (Atom x) (Atom y) = equivAtom x y
equivExpr (Call f xs) (Call g ys) =
  (&&) <$> equivAtom f g <*> andM (zipWith equivAtom xs ys)
equivExpr (CallDirect f xs) (CallDirect g ys) = do
  subst <- ask
  ( HashMap.lookupDefault (Var f) f subst
      == HashMap.lookupDefault (Var g) g subst
      &&
    )
    <$> andM (zipWith equivAtom xs ys)
equivExpr (RawCall n t xs) (RawCall m u ys) =
  ((n == m && t == u) &&) <$> andM (zipWith equivAtom xs ys)
equivExpr (Cast t x) (Cast u y) =
  (t == u &&) <$> equivAtom x y
equivExpr (Let ds e) (Let ds' e') = do
  subst <- mconcat <$> zipWithM equivLocalDef ds ds'
  if HashMap.null subst
    then pure False
    else do
      local (subst <>) $ equivExpr e e'
equivExpr (Match e cs) (Match e' cs') = do
  (&&) <$> equivExpr e e' <*> andM (zipWith equivCase cs cs')
equivExpr (Switch x cs e) (Switch y cs' e') = do
  andM [equivAtom x y, andM $ zipWith equivAlt cs cs', equivExpr e e']
  where
    equivAlt (tag, e) (tag', e') | tag == tag' = equivExpr e e'
    equivAlt _ _ = pure False
equivExpr (SwitchUnboxed x cs e) (SwitchUnboxed y cs' e') = do
  andM [equivAtom x y, andM $ zipWith equivAlt cs cs', equivExpr e e']
  where
    equivAlt (tag, e) (tag', e') | tag == tag' = equivExpr e e'
    equivAlt _ _ = pure False
equivExpr (Destruct x c ps e) (Destruct y c' ps' e') | c == c' = do
  (&&)
    <$> equivAtom x y
    <*> local
      (HashMap.fromList (zip ps $ map Var ps') <>)
      (equivExpr e e')
equivExpr (DestructRecord x kps e) (DestructRecord y kps' e') = do
  (&&)
    <$> equivAtom x y
    <*> local
      (HashMap.fromList (zip (HashMap.elems kps) (map Var $ HashMap.elems kps')) <>)
      (equivExpr e e')
equivExpr (Assign x e b) (Assign y e' b') =
  (&&)
    <$> equivExpr e e'
    <*> local (HashMap.insert x (Var y)) (equivExpr b b')
equivExpr (Error t) (Error u) = pure $ t == u
equivExpr _ _ = pure False

equivCase :: (MonadReader Subst m) => Case (Id Type) -> Case (Id Type) -> m Bool
equivCase (Unpack c ps e) (Unpack c' ps' e') | c == c' = do
  local (HashMap.fromList (zip ps $ map Var ps') <>) $ equivExpr e e'
equivCase (OpenRecord kps e) (OpenRecord kps' e') = do
  local
    (HashMap.fromList (zip (HashMap.elems kps) (map Var $ HashMap.elems kps')) <>)
    $ equivExpr e e'
equivCase (Exact u e) (Exact u' e')
  | u == u' =
      equivExpr e e'
equivCase (Bind x t e) (Bind y u f) | t == u = do
  local (HashMap.insert x (Var y)) $ equivExpr e f
equivCase _ _ = pure False

equivLocalDef :: (MonadReader Subst m) => LocalDef (Id Type) -> LocalDef (Id Type) -> m Subst
equivLocalDef (LocalDef x t o) (LocalDef y u p) =
  local (HashMap.insert x (Var y))
    $ ifM
      ((t == u &&) <$> equivObj o p)
      (pure $ HashMap.singleton x (Var y))
      (pure mempty)

equivObj :: (MonadReader Subst m) => Obj (Id Type) -> Obj (Id Type) -> m Bool
equivObj (Fun ps e) (Fun ps' e') = do
  local (HashMap.fromList (zip ps $ map Var ps') <>) $ equivExpr e e'
equivObj (Pack t c xs) (Pack u d ys) =
  ((t == u && c == d) &&) <$> andM (zipWith equivAtom xs ys)
equivObj (Record kvs) (Record kvs') =
  andM $ HashMap.intersectionWith equivAtom kvs kvs'
equivObj _ _ = pure False

equivAtom :: (MonadReader Subst m) => Atom (Id Type) -> Atom (Id Type) -> m Bool
equivAtom (Var x) (Var y) = do
  subst <- ask
  pure
    $ HashMap.lookupDefault (Var x) x subst
    == HashMap.lookupDefault (Var y) y subst
equivAtom x y = pure $ x == y
