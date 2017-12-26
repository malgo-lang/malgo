{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Malgo.KNormal where

import           Control.Monad.Except
import           Control.Monad.State
-- import           Data.String
import           Language.Malgo.HIR       hiding (_externs)
import           Language.Malgo.Rename    (ID (..), RnEnv (..))
import qualified Language.Malgo.Syntax    as S
import           Language.Malgo.TypeCheck
import           Language.Malgo.Utils
import           Text.PrettyPrint

data KEnv = KEnv { _externs :: [Extern TypedID]
                 , _count   :: Int
                 }
  deriving Show

initKEnv :: RnEnv -> KEnv
initKEnv (RnEnv i _ _) = KEnv [] i

type KNormal a = Malgo KEnv a

runKNormal :: RnEnv -> KNormal a -> (Either MalgoError a, KEnv)
runKNormal env m = runMalgo m (initKEnv env)

knormal
  :: RnEnv
     -> S.Expr TypedID
     -> (Either MalgoError (Program TypedID), KEnv)
knormal env e = runKNormal env $ do
  e' <- transExpr (flattenLet e)
  exs <- gets _externs
  return (Program exs [] e')

throw :: Info -> Doc -> KNormal a
throw info mes = throwError (KNormalError info mes)

addEx :: TypedID -> String -> KNormal ()
addEx name orig =
  modify $ \e -> e { _externs = ExDec name orig : _externs e }

flattenLet :: S.Expr TypedID -> S.Expr TypedID
flattenLet (S.Let info [decl] body) =
  S.Let info [decl] body
flattenLet (S.Let info (d:ds) body) =
  S.Let info [d] (flattenLet (S.Let info ds body))
flattenLet e = e

newTmp :: S.Type -> KNormal TypedID
newTmp typ = do
  c <- gets _count
  modify $ \e -> e { _count = c + 1 }
  return (TypedID (Internal "$k" c) typ)

newUnused :: KNormal TypedID
newUnused = do
  c <- gets _count
  modify $ \e -> e { _count = c + 1 }
  return (TypedID (Internal "$_" c) "Unit")

transOp :: S.Op -> S.Type -> KNormal Op
transOp S.Add _  = return Add
transOp S.Sub _  = return Sub
transOp S.Mul _  = return Mul
transOp S.Div _  = return Div
transOp S.FAdd _ = return FAdd
transOp S.FSub _ = return FSub
transOp S.FMul _ = return FMul
transOp S.FDiv _ = return FDiv
transOp S.Mod _  = return Mod
transOp S.Eq ty  = return $ Eq ty
transOp S.Neq ty = return $ Neq ty
transOp S.Lt ty  = return $ Lt ty
transOp S.Gt ty  = return $ Gt ty
transOp S.Le ty  = return $ Le ty
transOp S.Ge ty  = return $ Ge ty
transOp S.And _  = return And
transOp S.Or _   = return Or

insertLet :: S.Expr TypedID -> (TypedID -> KNormal (Expr TypedID)) -> KNormal (Expr TypedID)
insertLet v k = do
  x <- newTmp (typeOf v)
  v' <- transExpr v
  e <- k x
  return (Let (ValDec x v') e)

transExpr :: S.Expr TypedID -> KNormal (Expr TypedID)
transExpr (S.Var _ x)    = return (Var x)
transExpr (S.Int _ x)    = return (Int x)
transExpr (S.Float _ x)  = return (Float x)
transExpr (S.Bool _ x)   = return (Bool x)
transExpr (S.Char _ x)   = return (Char x)
transExpr (S.String _ x) = return (String x)
transExpr (S.Unit _)     = return Unit
transExpr (S.Call _ fn args) =
  bind args [] (\args' -> return $ Call fn args' [])
  where bind [] args' k     = k (reverse args')
        bind (x:xs) args' k = insertLet x (\x' -> bind xs (x':args') k)
transExpr (S.BinOp _ op e1 e2) = do
  op' <- transOp op (typeOf e1)
  insertLet e1 (\x -> insertLet e2 (\y -> return (BinOp op' x y)))
transExpr (S.If _ c t f) =
  insertLet c (\c' -> do
                  t' <- transExpr t
                  f' <- transExpr f
                  return (If c' t' f'))
transExpr (S.Let _ [S.ValDec _ name _ val] body) = do
  val' <- transExpr val
  body' <- transExpr body
  return (Let (ValDec name val') body')
transExpr (S.Let _ [S.FunDec _ fn params _ fbody] body) = do
  fbody' <- transExpr fbody
  body' <- transExpr body
  return (Let (FunDec fn (map fst params) [] fbody') body')
transExpr (S.Let _ [S.ExDec _ name _ orig] body) = do
  addEx name orig
  transExpr body
transExpr (S.Seq _ e1 e2) = do
  unused <- newUnused
  e1' <- transExpr e1
  e2' <- transExpr e2
  return $ Let (ValDec unused e1') e2'
transExpr (S.Let info _ _) =
  throw info (text "unreachable")
