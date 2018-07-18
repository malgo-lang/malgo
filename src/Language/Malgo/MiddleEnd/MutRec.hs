{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.MutRec (remove, lint) where

import           Data.List                      (nubBy)
import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import           Language.Malgo.Monad
import           Lens.Micro.Platform            (_1)
import           RIO
import qualified RIO.List                       as L
import qualified RIO.Map                        as Map
import           System.Exit
import           Text.PrettyPrint.HughesPJClass hiding ((<>))

perm :: Eq a => [a] -> [[a]]
perm xs = filter (not . null)
          $ nubBy (\x y -> L.headMaybe x == L.headMaybe y)
          $ L.permutations xs

type Env = Map (ID MType) (ID MType)

remove :: Expr (ID MType) -> RIO MalgoApp (Expr (ID MType))
remove e = runReaderT (removeMutRec e) Map.empty

renameID :: ID MType -> ReaderT Env (RIO MalgoApp) (ID MType)
renameID (ID name _ meta) = newID meta name

updateID :: ID MType -> ReaderT Env (RIO MalgoApp) (ID MType)
updateID i = do
  env <- ask
  case Map.lookup i env of
    Just x -> return x
    Nothing -> liftApp $ do logError "unreachable(removeMutRec)"
                            liftIO exitFailure

updateFunDecs :: [(ID MType, Maybe [ID MType], Expr (ID MType))] -> ReaderT Env (RIO MalgoApp) [(ID MType, Maybe [ID MType], Expr (ID MType))]
updateFunDecs [] = return []
updateFunDecs ((f, mparams, fbody):xs) = do
  f' <- renameID f
  mparams' <- case mparams of
                Just params -> Just <$> mapM renameID params
                Nothing     -> return Nothing
  local (Map.fromList (zip (f:fromMaybe [] mparams) (f':fromMaybe [] mparams')) <>) $ do
    fbody' <- removeMutRec fbody
    xs' <- updateFunDecs xs
    return $ (f', mparams', fbody'):xs'

removeMutRec :: Expr (ID MType) -> ReaderT Env (RIO MalgoApp) (Expr (ID MType))
removeMutRec (Var a) = Var <$> updateID a
removeMutRec (LetRec fs body) = do
  let fss = map consFunDecs $ perm fs
  fss' <- map consFunDecs <$> mapM updateFunDecs fss
  let vm = zip (map (view _1 . head') fss) (map (view _1 . head') fss')
  local (Map.fromList vm <>) $ do
    body' <- removeMutRec body
    return $ L.foldl (flip LetRec) body' fss'
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
removeMutRec e = return e

consFunDecs :: [(a, Maybe [a], Expr a)] -> [(a, Maybe [a], Expr a)]
consFunDecs [] = []
consFunDecs [x] = [x]
consFunDecs ((f, mparams, fbody):xs) =
  [(f, mparams, LetRec (consFunDecs xs) fbody)]

lint :: Pretty a => Expr a -> Either Doc ()
lint (LetRec fs body) =
  case fs of
    [(_, _, fbody)] -> lint fbody >> lint body
    _               -> Left $ "invalid FunDecs:" <+> pPrint fs
lint (Let _ val body) = lint val >> lint body
lint (If _ t f) = lint t >> lint f
lint _ = return ()
