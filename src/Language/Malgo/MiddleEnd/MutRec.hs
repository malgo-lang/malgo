{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Language.Malgo.MiddleEnd.MutRec (removeMutRec, optimizeFunDecs) where

import Data.List (nubBy)
import Language.Malgo.Monad
import Language.Malgo.Prelude
import Language.Malgo.IR.IR
import Language.Malgo.ID

optimizeFunDecs :: a
optimizeFunDecs = undefined

perm :: Eq a => [a] -> [[a]]
perm xs = filter notNull $ nubBy f $ permutations xs
  where f [] _ = False
        f _ [] = False
        f (x:_) (y:_) = x == y
        notNull [] = False
        notNull _ = True

data Env = Env { _varmap :: Map (ID MType) (ID MType)
               , _uniqSupply :: UniqSupply
               }

makeLenses ''Env

instance MalgoEnv Env where
  uniqSupplyL = uniqSupply
  genEnv = return . Env mempty

renameID :: MonadMalgo Env m => ID MType -> m (ID MType)
renameID (ID name _ meta) = do
  u <- newUniq
  return (ID name u meta)

updateID :: MonadMalgo Env m => ID MType -> m (ID MType)
updateID id =
  lookupTable "unreachable(removeMutRec)" id varmap

updateFunDecs :: MonadMalgo Env m => [(ID MType, Maybe [ID MType], Expr (ID MType))] -> m [(ID MType, Maybe [ID MType], Expr (ID MType))]
updateFunDecs [] = return []
updateFunDecs ((f, mparams, fbody):xs) = do
  f' <- renameID f
  mparams' <- case mparams of
                Just params -> Just <$> mapM renameID params
                Nothing -> return Nothing
  addTable (zip (f:fromMaybe [] mparams) (f':fromMaybe [] mparams')) varmap $ do
    fbody' <- removeMutRec fbody
    xs' <- updateFunDecs xs
    return $ (f', mparams', fbody'):xs'

removeMutRec :: MonadMalgo Env m => Expr (ID MType) -> m (Expr (ID MType))
removeMutRec (Var a) = Var <$> updateID a
removeMutRec (LetRec fs body) = do
  let fss = map consFunDecs $ perm fs
  fss' <- map consFunDecs <$> mapM updateFunDecs fss
  let vm = zip (map (view _1 . head') fss) (map (view _1 . head') fss')
  addTable vm varmap $ do
    body' <- removeMutRec body
    return $ foldl (flip LetRec) body' fss'
  where head' (x:_) = x
        head' _ = error "unreachable(head)"
removeMutRec (Tuple xs) = Tuple <$> mapM updateID xs
removeMutRec (Apply f args) = Apply <$> updateID f <*> mapM updateID args
removeMutRec (Let n val body) = do
  n' <- renameID n
  addTable [(n, n')] varmap $ Let n' <$> removeMutRec val <*> removeMutRec body
removeMutRec (Cast ty a) = Cast ty <$> updateID a
removeMutRec (Access a xs) = Access <$> updateID a <*> pure xs
removeMutRec (If c t f) = If <$> updateID c <*> removeMutRec t <*> removeMutRec f
removeMutRec e = return e

consFunDecs :: [(ID MType, Maybe [ID MType], Expr (ID MType))] -> [(ID MType, Maybe [ID MType], Expr (ID MType))]
consFunDecs [] = []
consFunDecs [x] = [x]
consFunDecs ((f, mparams, fbody):xs) =
  [(f, mparams, LetRec (consFunDecs xs) fbody)]
