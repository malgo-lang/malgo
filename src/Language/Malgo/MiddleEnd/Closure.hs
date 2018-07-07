{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Malgo.MiddleEnd.Closure (trans, knownFuns) where

import           Control.Lens.TH
import           Control.Monad.State
import           Language.Malgo.FreeVars
import           Language.Malgo.ID       hiding (newID)
import           Language.Malgo.IR.IR
import           Language.Malgo.Monad
import           Language.Malgo.Prelude  ()
import           RIO
import           RIO.List                ((\\))
import qualified RIO.Map                 as Map

data Env = Env { _varmap :: Map (ID MType) (ID MType)
               , _knowns :: [ID MType]
               }

makeLenses ''Env

trans :: Expr (ID MType) -> RIO MalgoApp (Program (ID MType))
trans e = flip execStateT (Program []) $ flip runReaderT (Env Map.empty (knownFuns e)) $ do
  u <- lift $ lift newUniq'
  let mainFun = ID "main" u (FunctionTy (IntTy 32) [])
  e' <- transExpr e
  addDefn (DefFun mainFun [] e')

addDefn :: MonadState (Program a) m => Defn a -> m ()
addDefn defn = do
  Program xs <- get
  put (Program $ defn : xs)

newID :: Text -> a -> ReaderT Env (StateT (Program (ID MType)) (RIO MalgoApp)) (ID a)
newID name meta = do
  u <- lift $ lift newUniq'
  return $ ID name u meta

updateID :: MonadReader Env m => ID MType -> m (ID MType)
updateID a =
  fromMaybe a . Map.lookup a <$> view varmap

transExpr :: Expr (ID MType) -> ReaderT Env (StateT (Program (ID MType)) (RIO MalgoApp)) (Expr (ID MType))
transExpr (Var a)    = Var <$> updateID a
transExpr (Tuple xs) = Tuple <$> mapM updateID xs
transExpr (Apply f args) = do
  f' <- updateID f
  k <- view knowns
  if f' `elem` k
    then Apply f' <$> mapM updateID args
    else insertLet "fn" (Access f' [0, 0])
         (\fn -> insertLet "env" (Access f' [0, 1])
           (\env -> return $ Apply fn (env : args)))
  where insertLet name e k = do
          i <- newID name (mTypeOf e)
          Let i e <$> k i
transExpr (Let n val body) = do
  val' <- transExpr val
  let n' = set idMeta (mTypeOf val') n
  local (over varmap $ Map.insert n n') $
    Let n' val' <$> transExpr body
transExpr (LetRec [(fn, mparams, fbody)] body) = do
  ks <- view knowns
  let params = map (over idMeta packFunTy) (fromMaybe [] mparams)
  if fn `elem` ks
    then do let fn' = set idMeta (FunctionTy (packFunTy $ mTypeOf fbody) (map (view idMeta) params)) fn
            local (over varmap (Map.fromList ((fn, fn'):zip (fromMaybe [] mparams) params) <>)) $ do
              fbody' <- transExpr fbody
              addDefn (DefFun fn' (fromMaybe [] mparams) fbody')
              transExpr body
    else do let cs = fv fbody \\ (fn : fromMaybe [] mparams)
            clsid <- newID (view idName fn <> "$cls") (packFunTy (view idMeta fn))
            let fn' = set idMeta (FunctionTy (packFunTy $ mTypeOf fbody) (PointerTy (IntTy 8) : map (view idMeta) params)) fn
            cap <- newID (view idName fn <> "$cap") (PointerTy (IntTy 8))
            local (over varmap (Map.fromList ((fn, clsid) : zip (fromMaybe [] mparams) params) <>)) $ do
              cs' <- mapM updateID cs
              cs'' <- mapM (\i -> newID (view idName i) (view idMeta i)) cs'
              cap' <- newID (view idName fn <> "$cap_unpacked") (PointerTy (StructTy (map (view idMeta) cs'')))
              fbody' <- local (over varmap (Map.fromList (zip cs cs'') <>)) $ transExpr fbody
              body' <- transExpr body
              defn <- DefFun fn' (cap:fromMaybe [] mparams) . Let cap' (Cast (view idMeta cap') cap) <$> makeLet cap' 0 cs'' fbody'
              addDefn defn
              fvid <- newID "fv" (PointerTy (StructTy (map (view idMeta) cs')))
              fvid' <- newID "fv" (PointerTy $ IntTy 8)
              return $ Let fvid (Tuple cs') $ Let fvid' (Cast (PointerTy $ IntTy 8) fvid) $ Let clsid (Tuple [fn', fvid']) body'
  where makeLet _ _ [] e = return e
        makeLet cap i (x:xs) e =
          Let x (Access cap [0, i]) <$> makeLet cap (i+1) xs e
transExpr LetRec{} = undefined
transExpr (Cast ty a) = Cast ty <$> updateID a
transExpr (Access a xs) = Access <$> updateID a <*> pure xs
transExpr (If c t f) = If <$> updateID c <*> transExpr t <*> transExpr f
transExpr e = return e

packFunTy :: MType -> MType
packFunTy (FunctionTy ret params) = PointerTy $ StructTy [FunctionTy (packFunTy ret) (PointerTy (IntTy 8) : map packFunTy params), PointerTy (IntTy 8)]
packFunTy (PointerTy ty) = PointerTy $ packFunTy ty
packFunTy (StructTy xs) = StructTy $ map packFunTy xs
packFunTy t = t

knownFuns :: Expr (ID MType) -> [ID MType]
knownFuns (LetRec [(fn, mparams, fbody)] body) =
  if null (fv fbody \\ fromMaybe [] mparams)
    then fn : (knownFuns fbody <> knownFuns body)
    else knownFuns fbody <> knownFuns body
knownFuns (Let x Prim{} e) = x : knownFuns e
knownFuns (Let _ v e) = knownFuns v <> knownFuns e
knownFuns (If _ t f) = knownFuns t <> knownFuns f
knownFuns _ = []
