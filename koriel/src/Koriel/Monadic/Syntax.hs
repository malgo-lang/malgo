{-# LANGUAGE UndecidableInstances #-}

module Koriel.Monadic.Syntax where

import qualified Data.List as List
import Data.Monoid
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude hiding (exp)
import Koriel.Pretty
import qualified RIO.HashSet as HashSet
import qualified RIO.List.Partial as Partial

data Program = Program {topVars :: [(Var, Exp)], topFuncs :: [(Var, Func)]}
  deriving stock (Show, Eq, Ord, Generic)

data Func = Func [Var] Exp
  deriving stock (Show, Eq, Ord, Generic)

data Exp
  = Bind Exp Var Exp
  | Unit Value
  | Match Value [(Value, Exp)]
  | Apply Var [Value]
  | Alloc Value
  | Fetch Var (Maybe Int)
  | Update Var Value
  | Cast Type Value -- for polymorphic and/or recursive types
  deriving stock (Show, Eq, Ord, Generic)

freevars :: Exp -> HashSet Var
freevars (Bind e1 x e2) = freevars e1 <> HashSet.delete x (freevars e2)
freevars (Unit v) = freevars' v
freevars (Match v cs) = freevars' v <> mconcat (map (\(pat, expr) -> HashSet.difference (freevars expr) (freevars' pat)) cs)
freevars (Apply f xs) = HashSet.insert f $ mconcat $ map freevars' xs
freevars (Alloc v) = freevars' v
freevars (Fetch v _) = HashSet.singleton v
freevars (Update var value) = HashSet.insert var $ freevars' value
freevars (Cast _ v) = freevars' v

freevars' :: Value -> HashSet Var
freevars' (Pack _ values) = mconcat $ map freevars' values
freevars' (Var var) = HashSet.singleton var
freevars' _ = HashSet.empty

data Value
  = Pack Tag [Value]
  | Int32 Int32
  | Int64 Int64
  | Float Float
  | Double Double
  | Char Char
  | String String
  | Var Var
  deriving stock (Show, Eq, Ord, Generic)

type Tag = Int

type Var = Id Type

data Type
  = TInt32
  | TInt64
  | TFloat
  | TDouble
  | TChar
  | TString
  | TAny
  | TFun [Type] Type
  | TNode Tag [Type]
  | TPtr Type
  | TUnion Type Type
  | TEmpty -- for typeUnion and Update
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty Type where pPrint = text . show

typeUnion :: Type -> Type -> Type
typeUnion TEmpty t = t
typeUnion t TEmpty = t
typeUnion TAny _ = TAny
typeUnion _ TAny = TAny
typeUnion (TPtr a) (TPtr b) = TPtr (typeUnion a b)
typeUnion (TPtr _) t = error $ "It is not possible to construct the union of TPtr and " <> show t
typeUnion t (TPtr _) = error $ "It is not possible to construct the union of TPtr and " <> show t
typeUnion t1 t2
  | isSubType t1 t2 = t2
  | isSubType t2 t1 = t1
  | otherwise = TUnion t1 t2

isSubType :: Type -> Type -> Bool
isSubType t1 t2 = typeSub t1 t2 == TEmpty

typeSub :: Type -> Type -> Type
typeSub TEmpty _ = TEmpty
typeSub t1 TEmpty = t1
typeSub _ TAny = TEmpty
typeSub TAny _ = TAny
typeSub (TUnion t1 t2) x = typeUnion (typeSub t1 x) (typeSub t2 x)
typeSub x (TUnion t1 t2) = typeSub (typeSub x t1) t2
typeSub (TNode tag1 ts1) (TNode tag2 ts2)
  | tag1 == tag2 =
    if
        | allIsSubType -> TEmpty
        | anyIsEmpty -> TNode tag1 ts1
        | otherwise -> aux [] ts1 ts2
  where
    allIsSubType = List.and $ zipWith isSubType ts1 ts2
    anyIsEmpty = elem TEmpty $ zipWith typeIntersect ts1 ts2
    aux _ [] [] = TEmpty
    aux acc (s : ss) (w : ws) = typeUnion (TNode tag1 (acc <> [typeSub s w] <> ss)) (aux (s : acc) ss ws)
    aux _ _ _ = bug $ Unreachable "length ss == length ws"
typeSub t _ = t

typeIntersect :: Type -> Type -> Type
typeIntersect TEmpty _ = TEmpty
typeIntersect _ TEmpty = TEmpty
typeIntersect TAny t = t
typeIntersect t TAny = t
typeIntersect (TUnion t1 t2) x = typeUnion (typeIntersect t1 x) (typeIntersect t2 x)
typeIntersect x (TUnion t1 t2) = typeUnion (typeIntersect x t1) (typeIntersect x t2)
typeIntersect (TNode tag1 ts1) (TNode tag2 ts2) | tag1 == tag2 = TNode tag1 (zipWith typeIntersect ts1 ts2)
typeIntersect t1 t2
  | t1 `isSubType` t2 = t1
  | t1 `isSubType` t2 = t2
  | otherwise = TEmpty

class HasType a where
  typeOf :: a -> Type

instance HasType Func where
  typeOf (Func ps e) = TFun (map (view idMeta) ps) (typeOf e)

instance HasType Exp where
  typeOf (Bind _ _ e) = typeOf e
  typeOf (Unit v) = typeOf v
  typeOf (Match _ clauses) = Partial.foldr1 typeUnion $ map (typeOf . snd) clauses
  typeOf (Apply (view idMeta -> TFun _ ret) _) = ret
  typeOf Apply {} = bug $ Unreachable "typeOf Apply{} must be TFun _ _."
  typeOf (Alloc v) = TPtr $ typeOf v
  typeOf (Fetch (view idMeta -> TPtr ty) Nothing) = ty
  typeOf (Fetch (view idMeta -> TPtr (TNode _ ts)) (Just n))
    | length ts > n = ts Partial.!! n
    | otherwise = bug $ Unreachable "length ts must be larger than n."
  typeOf Fetch {} = bug $ Unreachable "Fetch can only be applied to TPtr."
  typeOf Update {} = TEmpty
  typeOf (Cast t _) = t

instance HasType Value where
  typeOf (Pack tag values) = TNode tag $ map typeOf values
  typeOf Int32 {} = TInt32
  typeOf Int64 {} = TInt64
  typeOf Float {} = TFloat
  typeOf Double {} = TDouble
  typeOf Char {} = TChar
  typeOf String {} = TString
  typeOf (Var var) = var ^. idMeta

newtype ExpBuilderT m a = ExpBuilderT {unExpBuilderT :: WriterT (Endo Exp) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadTrans, MonadState s, MonadReader r)

runExpBuilderT :: Functor f => ExpBuilderT f Exp -> f Exp
runExpBuilderT m = uncurry (flip appEndo) <$> runWriterT (unExpBuilderT m)

exp :: Functor f => ExpBuilderT f Exp -> f Exp
exp = runExpBuilderT

class Monad m => MonadExpBuilder m where
  bind :: String -> Exp -> m Var

instance (MonadIO m, HasUniqSupply env, MonadReader env m) => MonadExpBuilder (ExpBuilderT m) where
  bind hint exp = do
    x <- newInternalId ("$" <> hint) (typeOf exp)
    ExpBuilderT $ tell $ Endo $ Bind exp x
    pure x

unit :: MonadExpBuilder m => Value -> m Var
unit value = bind "unit" (Unit value)

match :: MonadExpBuilder m => Value -> [(Value, Exp)] -> m Var
match x cs = bind "match" (Match x cs)

apply :: MonadExpBuilder m => Var -> [Value] -> m Var
apply f xs = bind "apply" (Apply f xs)

alloc :: MonadExpBuilder m => Value -> m Var
alloc init = bind "alloc" (Alloc init)

fetch :: MonadExpBuilder m => Var -> Maybe Int -> m Var
fetch x idx = bind "fetch" (Fetch x idx)

update :: MonadExpBuilder m => Var -> Value -> m Var
update x value = bind "update" (Update x value)

cast :: MonadExpBuilder m => Type -> Value -> m Var
cast typ x = bind "cast" (Cast typ x)

-- | construct a closure value
closure ::
  ( MonadReader env m,
    HasUniqSupply env,
    MonadIO m,
    MonadExpBuilder m
  ) =>
  Func ->
  m (Id Type, Func, Var)
closure (Func params expr) = do
  -- calculate free variables
  let fvs = HashSet.toList $ freevars expr

  -- generate lambda-lifted function
  captureParam <- newInternalId "$capture" (TPtr TAny)
  liftedFunc <-
    Func (captureParam : params) <$> exp do
      captureParam <- cast (TPtr $ TNode 0 $ map (view idMeta) fvs) (Var captureParam)
      ifor_ fvs \i fv ->
        ExpBuilderT $ tell $ Endo $ Bind (Fetch captureParam (Just i)) fv
      pure expr
  liftedFuncName <- newInternalId "$closure" (typeOf liftedFunc)

  -- generate closure value
  captureArg <- cast (TPtr TAny) . Var =<< alloc (Pack 0 (map Var fvs))
  closureValue <- alloc (Pack 0 (Var captureArg : [Var liftedFuncName]))
  pure (liftedFuncName, liftedFunc, closureValue)

-- | destruct a closure value and apply arguments
applyClosure :: MonadExpBuilder m => Value -> [Value] -> m Var
applyClosure closure arguments =
  case typeOf closure of
    TPtr (TNode _ [TPtr TAny, TFun (TPtr TAny : _) _]) -> do
      closure <- unit closure
      capture <- fetch closure (Just 0)
      func <- fetch closure (Just 1)
      apply func (Var capture : arguments)
    _ -> bug $ Unreachable "invalid type"
