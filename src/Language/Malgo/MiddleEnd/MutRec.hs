{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
module Language.Malgo.MiddleEnd.MutRec (remove, lint) where

import           Data.List             (nubBy)
import qualified Data.Map.Strict       as Map
import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Universum

perm :: Eq a => [a] -> [[a]]
perm xs = filter (not . null)
          $ nubBy (\x y -> safeHead x == safeHead y)
          $ permutations xs

type Env = Map (ID MType) (ID MType)

remove :: MonadMalgo m => Expr (ID MType) -> m (Expr (ID MType))
remove e = runReaderT (removeMutRec e) mempty

renameID :: MonadMalgo m => ID MType -> m (ID MType)
renameID (ID name _ meta) = newID meta name

updateID :: (MonadReader Env m, MonadMalgo m) => ID MType -> m (ID MType)
updateID i = do
  env <- ask
  case Map.lookup i env of
    Just x  -> return x
    Nothing -> malgoError ("unreachable(removeMutRec):" $+$ show env $+$ pPrint i)

updateFunDecs :: (MonadReader Env m, MonadMalgo m) => [(ID MType, [ID MType], Expr (ID MType))] -> m [(ID MType, [ID MType], Expr (ID MType))]
updateFunDecs [] = return []
updateFunDecs ((f, params, fbody):xs) = do
  f' <- renameID f
  params' <- mapM renameID params
  local (Map.fromList (zip (f:params) (f':params')) <>) $ do
    fbody' <- removeMutRec fbody
    xs' <- updateFunDecs xs
    return $ (f', params', fbody'):xs'

removeMutRec :: (MonadReader Env m, MonadMalgo m) => Expr (ID MType) -> m (Expr (ID MType))
removeMutRec (Var a) = Var <$> updateID a
removeMutRec (LetRec fs body) = do
  let fss = map consFunDecs $ perm fs
  fss' <- map consFunDecs <$> mapM updateFunDecs fss
  let vm = zip (map (view _1 . head') fss) (map (view _1 . head') fss')
  local (Map.fromList vm <>) $ do
    body' <- removeMutRec body
    return $ foldl (flip LetRec) body' fss'
  where head' (x:_) = x
        head' _     = error "unreachable(head)"
removeMutRec (Tuple xs) = Tuple <$> mapM updateID xs
removeMutRec (Apply f args) = Apply <$> updateID f <*> mapM updateID args
removeMutRec (Let n val body) = do
  n' <- renameID n
  local (Map.insert n n') $ Let n' <$> removeMutRec val <*> removeMutRec body
removeMutRec (Cast ty a) = Cast ty <$> updateID a
removeMutRec (Access a xs) = Access <$> updateID a <*> pure xs
removeMutRec (If c t f) = If <$> updateID c <*> removeMutRec t <*> removeMutRec f
removeMutRec (Store a xs v) = Store <$> updateID a <*> pure xs <*> updateID v
removeMutRec (MakeArray ty size) = MakeArray ty <$> updateID size
removeMutRec (Read arr ix) = Read <$> updateID arr <*> updateID ix
removeMutRec (Write arr ix val) = Write <$> updateID arr <*> updateID ix <*> updateID val
removeMutRec e = return e

consFunDecs :: [(a, [a], Expr a)] -> [(a, [a], Expr a)]
consFunDecs [] = []
consFunDecs [x] = [x]
consFunDecs ((f, params, fbody):xs) =
  [(f, params, LetRec (consFunDecs xs) fbody)]

lint :: Pretty a => Expr a -> Either Doc ()
lint (LetRec fs body) =
  case fs of
    [(_, _, fbody)] -> lint fbody >> lint body
    _               -> Left $ "invalid FunDecs:" <+> pPrint fs
lint (Let _ val body) = lint val >> lint body
lint (If _ t f) = lint t >> lint f
lint _ = pass
