-- Based on: Fengyun Liu. 2016. A generic algorithm for checking exhaustivity of pattern matching (short paper). In Proceedings of the 2016 7th ACM SIGPLAN Symposium on Scala (SCALA 2016). Association for Computing Machinery, New York, NY, USA, 61–64. https://doi.org/10.1145/2998392.2998401
module Malgo.Refine.Space (Space (..), subspace, subtract, normalize, equalEmpty, buildUnion, HasSpace (..)) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (isSubsequenceOf)
import Data.List qualified as List
import Data.Traversable (for)
import Koriel.Id (Id)
import Koriel.Pretty hiding (space)
import Malgo.Infer.TypeRep
import Malgo.Prelude hiding (subtract)
import Malgo.Refine.RefineEnv
import Malgo.Syntax (Pat (..))
import Malgo.Syntax.Extension

-- | Space of values that covered by patterns
data Space
  = -- | empty space
    Empty
  | -- | type space e.g. Int32, Int32#, List a, Maybe Int32
    Type Type
  | -- | constructor space e.g. Nil, Cons a (List a), Just Int32
    Constructor (Id ()) [Space]
  | Tuple [Space]
  | Record [(Text, Space)]
  | -- | union of spaces
    Union Space Space
  deriving stock (Eq, Ord, Show)

instance Pretty Space where
  pPrint Empty = "O"
  pPrint (Type t) = "T" <> parens (pPrint t)
  pPrint (Constructor con ss) = "K" <> parens (sep $ punctuate "," $ pPrint con : map pPrint ss)
  pPrint (Tuple ss) = "Tuple" <> parens (sep $ punctuate "," $ map pPrint ss)
  pPrint (Record kss) = braces $ sep $ punctuate "," $ map (\(k, s) -> pPrint k <+> "=" <+> pPrint s) kss
  pPrint (Union s1 s2) = pPrint s1 <+> "|" <+> pPrint s2

-- | whether space s1 is a subspace of space s2
subspace :: MonadReader RefineEnv m => Space -> Space -> m Bool
subspace s1 s2 = subtract s1 s2 >>= \s' -> pure $ s' == Empty

-- malgoにはsubtypingがないので、これでいいはず
-- TODO: 検証。TyVar, TyBottomが気になる
isSubTypeOf :: Type -> Type -> Bool
isSubTypeOf t1 t2 = t1 == t2

normalize :: Space -> Space
normalize (Union s Empty) = normalize s
normalize (Union Empty s) = normalize s
normalize (Constructor con ss) = Constructor con $ map normalize ss
normalize (Tuple ss) = Tuple $ map normalize ss
normalize (Record kss) = Record $ map (second normalize) kss
normalize (Union s1 s2) = Union (normalize s1) (normalize s2)
normalize (Type t) = Type t
normalize Empty = Empty

buildUnion :: [Space] -> Space
buildUnion [] = Empty
buildUnion [s] = s
buildUnion (s : ss) = Union s (buildUnion ss)

-- Ref: Malgo.Infer.TypeRep.TyConApp

-- | Check whether the given type can be decomposed into space(s).
decomposable :: Type -> Bool
decomposable (TyConApp (TyCon _) _) = True
decomposable (TyConApp (TyTuple _) _) = True
decomposable (TyRecord _) = True
decomposable _ = False

decompose :: MonadReader RefineEnv m => Type -> m Space
decompose t@(TyConApp (TyCon con) ts) = do
  env <- asks (.typeDefEnv)
  case HashMap.lookup con env of
    Nothing -> pure $ Type t
    Just TypeDef {_typeConstructor, _typeParameters, _valueConstructors} -> do
      spaces <- traverse (constructorSpace $ HashMap.fromList $ zip _typeParameters ts) _valueConstructors
      pure $ buildUnion spaces
decompose (TyConApp (TyTuple _) ts) = do
  env <- ask
  let ss = map (space env) ts
  pure $ Tuple ss
decompose (TyRecord kts) = do
  env <- ask
  pure $
    Record $
      map (second $ space env) $
        -- sort by key because the order of `toList` results is unspecified.
        sortWith fst $
          HashMap.toList kts
decompose t = pure $ Type t

constructorSpace :: MonadReader RefineEnv m => HashMap TypeVar Type -> (Id (), Scheme Type) -> m Space
constructorSpace subst (con, Forall _ (splitTyArr -> (ps, _))) = do
  env <- ask
  let ss = map (space env . applySubst subst) ps
  pure $ Constructor con ss

isSuperOf :: Ord a => [(a, b)] -> [(a, b)] -> Bool
isSuperOf kts1 kts2
  | map fst kts1 `isSubsequenceOf` map fst kts2 = True
  | otherwise = False

-- | subtraction of s1 and s2
subtract :: MonadReader RefineEnv m => Space -> Space -> m Space
subtract Empty _ = pure Empty
subtract s1 Empty = pure s1
subtract (Type t1) (Type t2) | t1 `isSubTypeOf` t2 = pure Empty
subtract (Constructor k1 ss) (Constructor k2 ws)
  | k1 == k2 = subtract' (Constructor k1) ss ws
subtract (Tuple ss) (Tuple ws) = subtract' Tuple ss ws
subtract (Record kts1) (Record kts2)
  | kts2 `isSuperOf` kts1 = do
      kss <- for kts1 \(k, s1) -> do
        case List.lookup k kts2 of
          Nothing -> pure (k, Empty)
          Just s2 -> (k,) <$> s1 `subtract` s2
      isEmpty <- allM (equalEmpty . snd) kss
      if isEmpty
        then pure Empty
        else pure $ Record kss
  | otherwise =
      error $
        "Record kts2 is invalid pattern:\n"
          <> show kts1
          <> "\n"
          <> show kts2
          <> "\n"
subtract (Union s1 s2) x = Union <$> subtract s1 x <*> subtract s2 x
subtract x (Union s1 s2) = do
  s1' <- subtract x s1
  subtract s1' s2
subtract (Constructor _ _) (Type _) = pure Empty -- 型検査が通っているので、kはtのコンストラクタのはず
subtract (Type t) x | decomposable t = join $ subtract <$> decompose t <*> pure x
subtract x (Type t) | decomposable t = subtract x =<< decompose t
subtract a _ = pure a

subtract' :: MonadReader RefineEnv m => ([Space] -> Space) -> [Space] -> [Space] -> m Space
subtract' k ss ws = do
  isSubSpace <- and <$> zipWithM subspace ss ws
  isEmpty <- anyM equalEmpty =<< zipWithM intersection ss ws
  if
    | isSubSpace -> pure Empty
    | isEmpty -> pure $ k ss
    | otherwise -> aux [] ss ws
  where
    aux _ [] [] = pure Empty
    aux acc (s : ss) (w : ws) = do
      s' <- subtract s w
      Union (k (acc <> [s'] <> ss)) <$> aux (s : acc) ss ws
    aux _ _ _ = error "length ss == length ws"

-- | intersection of spaces
intersection :: MonadReader RefineEnv m => Space -> Space -> m Space
intersection Empty _ = pure Empty
intersection _ Empty = pure Empty
intersection (Type t1) (Type t2)
  | t1 `isSubTypeOf` t2 = pure $ Type t1
  | t2 `isSubTypeOf` t1 = pure $ Type t2
  | otherwise = pure Empty
intersection (Union s1 s2) x = Union <$> intersection s1 x <*> intersection s2 x
intersection x (Union s1 s2) = Union <$> intersection x s1 <*> intersection x s2
intersection (Constructor k1 ss) (Constructor k2 ws) | k1 == k2 = Constructor k1 <$> zipWithM intersection ss ws
intersection (Tuple ss) (Tuple ws) = Tuple <$> zipWithM intersection ss ws
intersection (Type t) x | decomposable t = join $ intersection <$> decompose t <*> pure x
intersection x (Type t) | decomposable t = intersection x =<< decompose t
intersection _ _ = pure Empty

-- | compare with empty space
equalEmpty :: MonadReader RefineEnv m => Space -> m Bool
equalEmpty Empty = pure True
equalEmpty (Type t)
  | decomposable t = equalEmpty =<< decompose t
  | otherwise = pure False
equalEmpty (Union s1 s2) = andM [equalEmpty s1, equalEmpty s2]
equalEmpty (Constructor _ ss) = anyM equalEmpty ss
equalEmpty (Tuple ss) = anyM equalEmpty ss
equalEmpty (Record (map snd -> ss)) = anyM equalEmpty ss

class HasSpace a where
  space :: RefineEnv -> a -> Space

instance HasSpace Type where
  space _ = Type

instance HasSpace (Pat (Malgo 'Refine)) where
  space _ (VarP x _) = Type $ typeOf x
  space env (ConP _ con ps) = Constructor con (map (space env) ps)
  space env (TupleP _ ps) = Tuple $ map (space env) ps
  space env (RecordP _ xps) = Record $ sort $ map (bimap identity (space env)) xps
  space _ (UnboxedP _ _) = Empty
