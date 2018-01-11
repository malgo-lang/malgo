{-# LANGUAGE FlexibleContexts #-}
module Language.Malgo.Closure (conv) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict          as Map
import           Data.String
import           Language.Malgo.FreeVars
import qualified Language.Malgo.HIR       as H
import           Language.Malgo.MIR
import           Language.Malgo.Rename    (ID (..))
import           Language.Malgo.Type
import           Language.Malgo.TypeCheck (TypedID (..))
import           Language.Malgo.Utils
import           Text.PrettyPrint


data ClsEnv = ClsEnv { _closures :: Map.Map TypedID TypedID
                     , _knowns   :: [TypedID]
                     , _fundecs  :: [FunDec TypedID]
                     , _count    :: Int
                     }
  deriving Show

initClsEnv :: Int -> ClsEnv
initClsEnv = ClsEnv Map.empty [] []

type ClsTrans a = Malgo ClsEnv a

runClosure :: Int -> ClsTrans a -> (Either MalgoError a, ClsEnv)
runClosure i m = runMalgo m (initClsEnv i)

conv
  :: Int -> H.Expr TypedID
     -> (Either MalgoError (Program TypedID), ClsEnv)
conv i x = runClosure i $ do
  x' <- convExpr x
  fs <- gets _fundecs
  return (Program fs x')

throw :: Doc -> ClsTrans a
throw = throwError . ClosureTransError

addKnown :: TypedID -> ClsTrans ()
addKnown name =
  modify $ \e -> e { _knowns = name : _knowns e }

addFunDec :: FunDec TypedID -> ClsTrans ()
addFunDec f =
  modify $ \e -> e { _fundecs = f : _fundecs e }

convID :: TypedID -> ClsTrans TypedID
convID name = do
  clss <- gets _closures
  case Map.lookup name clss of
    Nothing  -> return name
    Just cls -> return cls

addClsTrans :: TypedID -> TypedID -> ClsTrans ()
addClsTrans orig cls =
  modify $ \e -> e { _closures = Map.insert orig cls (_closures e) }

newClsID :: TypedID -> [TypedID] -> ClsTrans TypedID
newClsID (TypedID fn (FunTy params ret)) fv = do
  let ty = ClsTy params ret (map _type fv)
  c <- gets _count
  modify $ \e -> e { _count = c + 1 }
  return (TypedID
           (Internal (Language.Malgo.Rename._name fn `mappend` fromString "$cls") c)
           ty)
newClsID x _ = throw $ pretty x <+> text "is not function."

convExpr :: H.Expr TypedID -> ClsTrans (Expr TypedID)
convExpr (H.Var x)    = Var <$> convID x
convExpr (H.Int x)    = return (Int x)
convExpr (H.Float x)  = return (Float x)
convExpr (H.Bool x)   = return (Bool x)
convExpr (H.Char x)   = return (Char x)
convExpr (H.String x) = return (String x)
convExpr H.Unit       = return Unit
convExpr (H.Call fn args) = do
  closures <- gets _closures
  case Map.lookup fn closures of
    Nothing  -> CallDir fn <$> mapM convID args
    Just cls -> CallCls cls <$> mapM convID args
convExpr (H.If c t f) =
  If <$> convID c <*> convExpr t <*> convExpr f
convExpr (H.BinOp op x y) =
  BinOp op <$> convID x <*> convID y
-- convProgram (H.Program exs body) = do
--   mapM_ (addKnown . H._name) exs
--   convExterns exs
--   -- convLet body
--   t <- gets _revToplevel
--   m <- gets _revMain
--   return $ Program (reverse t) (reverse m)

-- convExterns :: [H.Extern TypedID] -> ClsTrans ()
-- convExterns = mapM_ convExtern

-- convExtern :: H.Extern TypedID -> ClsTrans ()
-- convExtern (H.ExDec name actual) = do
--   addKnown name
--   addToplevel $ ExDec name actual

-- -- convLet :: H.Expr TypedID -> ClsTrans ()
-- -- convLet (H.Let (H.ValDec var (H.String str)) rest) = do
-- --   addToplevel (StrDec var str)
-- --   convLet rest
-- -- convLet (H.Let (H.ValDec (TypedID var _) val) rest) = do
-- --   val' <- convExpr val
-- --   addMain (TypedID var (typeOf val') := val')
-- --   convLet rest
-- -- convLet (H.Let (H.FunDec fn params body) rest) = do
-- --   -- クロージャ変換して_revToplevelに追加
-- --   topLevelBackup <- gets _revToplevel
-- --   knownsBackup <- gets _knowns

-- --   -- fnに自由変数が無いと仮定してbodyをクロージャ変換
-- --   addKnown fn
-- --   mapM addKnown params
-- --   body' <- convLet body

-- --   knowns <- gets _knowns
-- --   let bodyFv = freevars body' \\ knowns
-- --   body' <- if empty bodyFv
-- --            then return body'
-- --            else do modify $ \e -> e { _revToplevel = _revToplevel e
-- --                                     , _knowns = knowns
-- --                                     }
-- --                    convLet body'
-- -- convLet x =
-- --   addMain (Do undefined)

-- convExpr (H.Call fn args) = do
--   closures <- gets _closures
--   case Map.lookup fn closures of
--     (Just fn') -> return $ CallCls fn' args
--     Nothing    -> return $ CallDir fn args
-- convExpr (H.If c t f) = If c <$> convExpr t <*> convExpr f
-- convExpr e@H.Let{} = throw . text $ "unreachable: " ++ show e
-- convExpr (H.Var x) = do
--   closures <- gets _closures
--   case Map.lookup x closures of
--     (Just x') -> return $ Var x'
--     Nothing   -> return $ Var x
-- convExpr (H.Int x) = return $ Int x
-- convExpr (H.Float x) = return $ Float x
-- convExpr (H.Bool x) = return $ Bool x
-- convExpr (H.Char x) = return $ Char x
-- convExpr e@H.String{} = throw . text $ "unreachable: " ++ show e
-- convExpr H.Unit = return Unit
-- convExpr (H.BinOp op x y) = return $ BinOp op x y
