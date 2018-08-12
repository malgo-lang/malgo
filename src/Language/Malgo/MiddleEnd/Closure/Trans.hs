{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Malgo.MiddleEnd.Closure.Trans (trans) where

import           Control.Lens          (makeLenses)
import           Data.List             ((\\))
import qualified Data.Map.Strict       as Map
import           Language.Malgo.ID     hiding (newID)
import qualified Language.Malgo.ID     as ID
import           Language.Malgo.IR.IR
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Universum

data Env = Env { _varmap :: Map (ID MType) (ID MType)
               , _knowns :: [ID MType]
               }

makeLenses ''Env

type TransM a = ReaderT Env (StateT (Program (ID MType)) MalgoM) a

trans :: Expr (ID MType) -> MalgoM (Program (ID MType))
trans e = flip execStateT (Program (ID "" (-1) (IntTy 0)) []) $ usingReaderT (Env mempty []) $ do
  u <- newUniq
  let mainFun = ID "main" u (FunctionTy (StructTy []) [])
  e' <- transExpr e
  addDefn (DefFun mainFun [] e')
  Program _ xs <- get
  put $ Program mainFun xs

addDefn :: MonadState (Program a) m => Defn a -> m ()
addDefn defn = do
  Program e xs <- get
  put (Program e $ defn : xs)

newID :: MonadMalgo f => Text -> a -> f (ID a)
newID name meta = ID.newID meta name

updateID :: ID MType -> TransM (ID MType)
updateID a = do
  ma <- Map.lookup a <$> view varmap
  case ma of
    Just a' -> return a'
    Nothing -> malgoError $ pPrint a <+> "is not defined(updateID)"

transExpr :: Expr (ID MType) -> TransM (Expr (ID MType))
transExpr (Var a)    = Var <$> updateID a
transExpr (Tuple xs) = Tuple <$> mapM updateID xs
transExpr (Apply f args) = do
  k <- view knowns
  if f `elem` k
    then Apply <$> updateID f <*> mapM updateID args
    else do f' <- updateID f
            insertLet "fn" (Access f' [0, 0])
              (\fn -> insertLet "env" (Access f' [0, 1])
                (\env -> Apply fn . (env:) <$> mapM updateID args))
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
transExpr (LetRec [(fn, params, fbody)] body) = do
  -- fnに自由変数がないと仮定してfbodyをクロージャ変換
  pgBackup <- get
  fbody' <- local (over knowns (fn:))
            $ local (over varmap (Map.fromList ((fn, fn') : zip params params') <>))
            $ transExpr fbody
  Program _ defs <- get
  if null (freevars fbody' \\ (params' ++ map _fnName defs)) && (fn `notElem` freevars body)
    -- 本当に自由変数がなければknownsに追加してbodyを変換
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
        params' = map packID params
        trans' pg = do
          put pg

          -- 再帰呼び出し用のクロージャ
          innerCls <- newID (view idName fn <> "$cls") (packFunTy $ view idMeta fn)
          -- fnをknownsに加えずにクロージャ変換
          fbody' <- local (over varmap (Map.fromList ((fn, innerCls) : zip params params') <>))
                    $ transExpr fbody
          Program _ defs <- get
          -- 自由変数のリスト
          let zs = freevars fbody' \\ (params' ++ map _fnName defs) -- すでに宣言されている関数名は自由変数にはならない
          -- 実際に変換後のfbody内で参照される自由変数のリスト
          zs' <- mapM transID (zs \\ [innerCls])

          -- 仮引数に追加されるポインタ
          capPtr <- newID "fv" $ PointerTy (IntTy 8)
          capPtr' <- newID "fv_unpacked" $ PointerTy
                     $ StructTy (map (view idMeta) $ zs \\ [innerCls])

          -- 自由変数zsを対応するfv_unpackedの要素zs'に変換
          let fbody'' = Let innerCls (Tuple [fn'', capPtr])
                        $ Let capPtr' (Cast (view idMeta capPtr') capPtr)
                        $ makeLet capPtr' 0 zs'
                        $ runReader (replace fbody') (Map.fromList (zip zs zs'))
          addDefn $ DefFun fn'' (capPtr : params') fbody''

          -- キャプチャされる値のタプル
          capTuple <- newID "capture" (PointerTy $ StructTy (map (view idMeta) zs'))
          capTuple' <- newID "capture_packed" (PointerTy $ IntTy 8)
          -- 生成されるクロージャ
          clsID <- newID (view idName fn <> "$cls") (packFunTy (mTypeOf fn))

          body' <- local (over varmap (Map.insert fn clsID)) (transExpr body)
          return $ Let capTuple (Tuple $ zs \\ [innerCls])
            $ Let capTuple' (Cast (PointerTy $ IntTy 8) capTuple)
            $ Let clsID (Tuple [fn'', capTuple']) body'
        transID x = newID (view idName x) (view idMeta x)
        makeLet _ _ [] e = e
        makeLet cap i (x:xs) e =
          Let x (Access cap [0, i]) $ makeLet cap (i+1) xs e
transExpr LetRec{} = malgoError "mutative recursion must be removed by Language.Malgo.MiddleEnd.MutRec"
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

replace' :: (Ord b, MonadReader (Map b b) f) => b -> f b
replace' a = fromMaybe a . Map.lookup a <$> ask

replace :: (Ord a, MonadReader (Map a a) f) => Expr a -> f (Expr a)
replace (Var a)        = Var <$> replace' a
replace (Tuple xs)     = Tuple <$> mapM replace' xs
replace (Apply f args) = Apply f <$> mapM replace' args
replace (Let n v e)    = Let n <$> replace v <*> replace e
replace LetRec{}       = error "unreachable"
replace (Cast ty a)    = Cast ty <$> replace' a
replace (Access a xs)  = Access <$> replace' a <*> pure xs
replace (If c t f)     = If <$> replace' c <*> replace t <*> replace f
replace e              = return e
