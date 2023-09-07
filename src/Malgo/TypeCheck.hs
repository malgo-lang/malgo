module Malgo.TypeCheck (typeCheck) where

import Control.Lens ((??))
import Control.Monad (replicateM)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (MonadState (get), StateT (runStateT), modify)
import Control.Monad.Trans.Writer.CPS (runWriterT)
import Control.Monad.Writer.CPS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Malgo.Monad
import Malgo.Prelude
import Malgo.Syntax
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import Prettyprinter qualified as P
import Text.Parsec (SourcePos)

type TypeError = Doc ()

unifyError :: (Pretty a1, Pretty a2) => SourcePos -> a1 -> a2 -> Doc ()
unifyError pos t1 t2 = pretty pos <> ": cannot unify " <> pretty t1 <> " and " <> pretty t2

solveLimitError :: SourcePos -> Doc ()
solveLimitError pos = pretty pos <> ": solve limit exceeded"

notDefinedError :: SourcePos -> Id -> Doc ()
notDefinedError pos name = pretty pos <> ":" <+> pretty name <+> "is not defined"

type TypeCtx = Map Id Type

data Type
  = -- | type variable, e.g. @a@ in @id : a -> a@
    VarTy Id
  | IntTy
  | FunTy [Type] Type
  | -- | meta variable used in unification
    MetaTy Id
  deriving stock (Eq, Ord, Show, Generic)

instance Pretty Type where
  pretty (VarTy x) = pretty x
  pretty IntTy = "Int"
  pretty (FunTy ts t) = P.parens (P.hsep (P.punctuate "," (pretty <$> ts)) P.<+> "->" P.<+> pretty t)
  pretty (MetaTy x) = pretty x

data Scheme = Forall [Id] Type
  deriving stock (Eq, Ord, Show, Generic)

instantiate :: (MonadMalgo m) => Scheme -> m Type
instantiate (Forall xs t) = do
  ys <- traverse (\x -> MetaTy <$> newId x.name) xs
  pure $ replace (Map.fromList $ zip xs ys) t
  where
    replace s (VarTy x) = fromMaybe (VarTy x) (Map.lookup x s)
    replace _ IntTy = IntTy
    replace s (FunTy params ret) = FunTy (replace s <$> params) (replace s ret)
    replace _ (MetaTy x) = MetaTy x

newtype Subst = Subst {subst :: Map Id Type}
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable Type where
  apply _ (VarTy x) = VarTy x
  apply _ IntTy = IntTy
  apply s (FunTy ts t) = FunTy (apply s <$> ts) (apply s t)
  apply s (MetaTy x) = fromMaybe (MetaTy x) (Map.lookup x s.subst)

instance Substitutable Subst where
  apply s1 s2 = Subst (fmap (apply s1) s2.subst `Map.union` s1.subst)

data Constraint = Equal SourcePos Type Type
  deriving stock (Eq, Ord, Show, Generic)

instance Substitutable Constraint where
  apply s (Equal pos t1 t2) = Equal pos (apply s t1) (apply s t2)

instance (Substitutable a) => Substitutable [a] where
  apply = fmap . apply

unify :: (MonadError TypeError m) => SourcePos -> Type -> Type -> m (Subst, [Constraint])
unify _ (VarTy x) (VarTy y) | x == y = pure (mempty, [])
unify _ IntTy IntTy = pure (mempty, [])
unify pos (FunTy ts1 t1) (FunTy ts2 t2) = pure (mempty, zipWith (Equal pos) (t1 : ts1) (t2 : ts2))
unify _ (MetaTy x) (MetaTy y)
  | x == y = pure (mempty, [])
  | otherwise = pure (Subst $ Map.singleton x (MetaTy y), [])
unify _ (MetaTy x) t = pure (Subst $ Map.singleton x t, [])
unify _ t (MetaTy x) = pure (Subst $ Map.singleton x t, [])
unify pos t1 t2 = throwError $ unifyError pos t1 t2

solve :: (MonadError TypeError m) => SourcePos -> [Constraint] -> m Subst
solve pos = solveLoop (5000 :: Int)
  where
    solveLoop n _ | n <= 0 = throwError $ solveLimitError pos
    solveLoop _ [] = pure mempty
    solveLoop n (Equal cpos t1 t2 : cs) = do
      (s, cs') <- unify cpos t1 t2
      s' <- solveLoop (n - 1) $ apply s (cs' <> cs)
      pure $ apply s' s

typeCheck :: (MonadMalgo m, MonadError TypeError m) => Expr Id -> m (Type, TypeCtx)
typeCheck expr = runStateT
  ?? mempty $ do
    (t, cs) <- runWriterT $ tcExpr expr
    subst <- solve (position expr) cs
    modify $ fmap (apply subst)
    pure $ apply subst t

tcExpr :: (MonadState TypeCtx m, MonadMalgo m, MonadError TypeError m) => Expr Id -> WriterT [Constraint] m Type
tcExpr (Var p name) = lookupType p name
tcExpr (Lit _ lit) = tcLit lit
tcExpr (App f args) = do
  tf <- tcExpr f
  targs <- traverse tcExpr args
  tret <- newMetaTy "a"
  tell [Equal (position f) tf (FunTy targs tret)]
  pure tret
tcExpr (Codata p clauses) = do
  tclauses <- traverse tcClause clauses
  tcodata <- newMetaTy "a"
  tell (map (Equal p tcodata) tclauses)
  pure tcodata

tcClause :: (MonadState TypeCtx m, MonadMalgo m, MonadError TypeError m) => Clause Id -> WriterT [Constraint] m Type
tcClause (Clause pattern body) = do
  thisTy <- newMetaTy "#"
  _ <- tcPat thisTy pattern
  tbody <- tcExpr body
  tparams <- replicateM (arityOf pattern) (newMetaTy "a")
  tell [Equal (position body) (FunTy tparams tbody) thisTy]
  pure thisTy

tcPat :: (MonadMalgo f, MonadWriter [Constraint] f, MonadState (Map Id Type) f) => Type -> Pat Id -> f Type
tcPat thisTy (PThis _) = pure thisTy
tcPat _ (PVar _ name) = do
  t <- newMetaTy (shorten name)
  modify $ Map.insert name t
  pure t
tcPat _ (PLit _ lit) = tcLit lit
tcPat thisTy (PApp f args) = do
  tf <- tcPat thisTy f
  targs <- traverse (tcPat thisTy) args
  tret <- newMetaTy "r"
  tell [Equal (position f) (FunTy targs tret) tf]
  pure tf

tcLit :: (Applicative m) => Literal -> m Type
tcLit (Int _) = pure IntTy

lookupType ::
  ( MonadError TypeError m,
    MonadState TypeCtx m
  ) =>
  SourcePos ->
  Id ->
  m Type
lookupType pos name = do
  ctx <- get
  case Map.lookup name ctx of
    Nothing -> throwError $ notDefinedError pos name
    Just t -> pure t

newMetaTy :: (MonadMalgo m) => Text -> m Type
newMetaTy name = MetaTy <$> newId ("'" <> name)