{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Malgo.MiddleEnd.Closure (trans) where

import           Control.Lens.TH
import           Control.Monad.State
import           Data.Text.Prettyprint.Doc
import           Language.Malgo.FreeVars
import           Language.Malgo.ID         hiding (newID)
import           Language.Malgo.IR.IR
import           Language.Malgo.Monad
import           Language.Malgo.Prelude    ()
import           RIO
import           RIO.List                  ((\\))
import qualified RIO.Map                   as Map
import           System.Exit

data Env = Env { _varmap :: Map (ID MType) (ID MType)
               -- , _closures :: Map (ID MType) (ID MType)
               , _knowns :: [ID MType]
               }

makeLenses ''Env

trans :: Expr (ID MType) -> RIO MalgoApp (Program (ID MType))
trans e = flip execStateT (Program (ID "" (-1) (IntTy 0)) []) $ flip runReaderT (Env Map.empty []) $ do
  u <- lift $ lift newUniq'
  let mainFun = ID "main" u (FunctionTy (IntTy 32) [])
  e' <- transExpr e
  addDefn (DefFun mainFun [] e')
  Program _ xs <- get
  put $ Program mainFun xs

addDefn :: MonadState (Program a) m => Defn a -> m ()
addDefn defn = do
  Program e xs <- get
  put (Program e $ defn : xs)

newID :: Text -> a -> ReaderT Env (StateT (Program (ID MType)) (RIO MalgoApp)) (ID a)
newID name meta = do
  u <- lift $ lift newUniq'
  return $ ID name u meta

updateID a = do
  ma <- Map.lookup a <$> view varmap
  case ma of
    Just a' ->
      return a'
    Nothing -> do lift $ lift $ logError $ displayShow $ pretty a <+> "is not defined(updateID)"
                  liftIO exitFailure

transExpr :: Expr (ID MType) -> ReaderT Env (StateT (Program (ID MType)) (RIO MalgoApp)) (Expr (ID MType))
transExpr (Var a)    = Var <$> updateID a
transExpr (Tuple xs) = Tuple <$> mapM updateID xs
transExpr (Apply f args) = do
  k <- view knowns
  if f `elem` k
    then Apply <$> updateID f <*> mapM updateID args
    else do f' <- updateID f
            insertLet "fn" (Access f' [0, 0])
              (\fn -> insertLet "env" (Access f' [0, 1])
                (\env -> return $ Apply fn (env : args)))
  where insertLet name e k = do
          i <- newID name (mTypeOf e)
          Let i e <$> k i
transExpr (Let n val@Prim{} body) =
    Let n val <$> local (over knowns (n:)) (local (over varmap $ Map.insert n n) (transExpr body))
transExpr (Let n val body) = do
  val' <- transExpr val
  let n' = set idMeta (mTypeOf val') n
  local (over varmap $ Map.insert n n') $
    Let n' val' <$> transExpr body
transExpr (LetRec [(fn, mparams, fbody)] body) = do
  -- fnに自由変数がないと仮定してfbodyをクロージャ変換
  pgBackup <- get
  fbody' <- local (over knowns (fn:))
            $ local (over varmap (Map.fromList ((fn, fn') : zip params params') <>))
            $ transExpr fbody
  Program _ defs <- get
  if null (fv fbody' \\ (params' ++ map _fnName defs))
    then do addDefn (DefFun fn' params' fbody')
            local (over knowns (fn:))
              $ local (over varmap (Map.insert fn fn'))
              $ transExpr body
    else trans' pgBackup

  where fn' =
          set idMeta
          (FunctionTy (packFunTy $ mTypeOf (Apply fn [])) (map (view idMeta) params')) fn
        fn'' =
          set idMeta
          (FunctionTy (packFunTy $ mTypeOf (Apply fn [])) (PointerTy (IntTy 8) : map (view idMeta) params')) fn
        params = fromMaybe [] mparams
        params' = map packID params
        trans' pg = do
          put pg
          innerCls <- newID (view idName fn <> "$cls") (packFunTy $ view idMeta fn)
          fbody' <- local (over varmap (Map.fromList ((fn, innerCls) : zip params params') <>))
                    $ transExpr fbody
          -- 自由変数のリスト
          Program _ defs <- get
          let zs = fv fbody' \\ (params' ++ map _fnName defs)
          -- 実際に変換後のfbody内で参照される自由変数のリスト
          zs' <- mapM transID (zs \\ [innerCls])

          -- 仮引数に追加されるポインタ
          capPtr <- newID "fv" $ PointerTy (IntTy 8)
          capPtr' <- newID "fv_unpacked" $ PointerTy
                     $ StructTy (map (view idMeta) $ zs \\ [innerCls])

          let fbody'' = Let innerCls (Tuple [fn'', capPtr])
                        $ Let capPtr' (Cast (view idMeta capPtr') capPtr)
                        $ makeLet capPtr' 0 zs'
                        $ replace (Map.fromList (zip zs zs')) fbody'
          addDefn $ DefFun fn'' (capPtr : params') fbody''

          -- キャプチャされる値のタプル
          capTuple <- newID "capture" (PointerTy $ StructTy (map (view idMeta) zs'))
          capTuple' <- newID "capture_packed" (PointerTy $ IntTy 8)
          clsID <- newID (view idName fn <> "$cls") (packFunTy (mTypeOf fn))

          body' <- local (over varmap (Map.insert fn clsID)) (transExpr body)
          return $ Let capTuple (Tuple $ zs \\ [innerCls])
            $ Let capTuple' (Cast (PointerTy $ IntTy 8) capTuple)
            $ Let clsID (Tuple [fn'', capTuple']) body'
        transID x = newID (view idName x) (view idMeta x)
        makeLet _ _ [] e = e
        makeLet cap i (x:xs) e =
          Let x (Access cap [0, i]) $ makeLet cap (i+1) xs e
transExpr LetRec{} = undefined
transExpr (Cast ty a) = Cast ty <$> updateID a
transExpr (Access a xs) = Access <$> updateID a <*> pure xs
transExpr (If c t f) = If <$> updateID c <*> transExpr t <*> transExpr f
transExpr e = return e

packID :: ID MType -> ID MType
packID x = set idMeta (packFunTy $ view idMeta x) x

packFunTy :: MType -> MType
packFunTy (FunctionTy ret params) = PointerTy $ StructTy [FunctionTy (packFunTy ret) (PointerTy (IntTy 8) : map packFunTy params), PointerTy (IntTy 8)]
packFunTy (PointerTy ty) = PointerTy $ packFunTy ty
packFunTy (StructTy xs) = StructTy $ map packFunTy xs
packFunTy t = t

replace' env a = fromMaybe a $ Map.lookup a env
replace env (Var a) = Var $ replace' env a
replace env (Tuple xs) = Tuple $ map (replace' env) xs
replace env (Apply f args) = Apply f $ map (replace' env) args
replace env (Let n v e) = Let n (replace env v) (replace env e)
replace _ LetRec{} = error "unreachable"
replace env (Cast ty a) = Cast ty (replace' env a)
replace env (Access a xs) = Access (replace' env a) xs
replace env (If c t f) = If (replace' env c) (replace env t) (replace env f)
replace _ e = e
