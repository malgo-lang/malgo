module Language.Malgo.Beta (betaTrans) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map.Strict          as Map
import           Data.Maybe
import           Language.Malgo.HIR
import           Language.Malgo.TypeCheck (TypedID (..))
import           Language.Malgo.Utils

newtype BEnv = BEnv { _table :: Map.Map TypedID TypedID }

initBEnv :: BEnv
initBEnv = BEnv Map.empty

type Beta a = Malgo BEnv a

runBeta :: Beta a -> (Either MalgoError a, BEnv)
runBeta m = runMalgo m initBEnv

betaTrans :: Program TypedID -> (Either MalgoError (Program TypedID), BEnv)
betaTrans prog = runBeta $ transProgram prog

addBind :: TypedID -> TypedID -> Beta ()
addBind x y =
  modify $ \e -> e { _table = Map.insert x y (_table e) }

find :: TypedID -> Beta TypedID
find x = do
  table <- gets _table
  let x' = Map.lookup x table
  return $ fromMaybe x x'

transProgram :: Program TypedID -> Beta (Program TypedID)
transProgram (Program exs tps body) = do
  tps' <- mapM transToplevel tps
  body' <- transExpr body
  return $ Program exs tps' body'

transToplevel :: Decl TypedID -> Beta (Decl TypedID)
transToplevel (FunDec fn params freevars fnbody) =
  FunDec fn params freevars <$> transExpr fnbody
transToplevel (ValDec name val) =
  ValDec name <$> transExpr val

transExpr :: Expr TypedID -> Beta (Expr TypedID)
transExpr (Let (FunDec fn params freevars fnbody) body) =
  Let <$> (FunDec fn params freevars <$> transExpr fnbody) <*> transExpr body
transExpr (Let (ValDec name val) body) = do
  val' <- transExpr val
  case val' of
    (Var x) ->
      addBind name x >> transExpr body
    _ -> Let (ValDec name val') <$> transExpr body
transExpr (BinOp op x y) =
  BinOp op <$> find x <*> find y
transExpr (If c t f) =
  If <$> find c <*> transExpr t <*> transExpr f
transExpr (Call fn args implicts) =
  Call <$> find fn <*> mapM find args <*> mapM find implicts
transExpr (Var x) = Var <$> find x
transExpr x = return x
