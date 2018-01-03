module Language.Malgo.LambdaLifting (lambdaLifting) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict          as Map
import           Language.Malgo.FreeVars
import           Language.Malgo.HIR
import           Language.Malgo.Syntax    (Type (..))
import           Language.Malgo.TypeCheck
import           Language.Malgo.Utils

data LLEnv = LLEnv { _table  :: Map.Map TypedID (TypedID, [TypedID])
                   , _knowns :: [TypedID]
                   }

initLLEnv = LLEnv Map.empty []

runLambdaLifting :: Malgo LLEnv a -> (Either MalgoError a, LLEnv)
runLambdaLifting m = runMalgo m initLLEnv

lambdaLifting x = runLambdaLifting (convProg x)

addKnown :: TypedID -> Malgo LLEnv ()
addKnown name =
  modify $ \e -> e { _knowns = name : _knowns e }

convProg :: Program TypedID -> Malgo LLEnv (Program TypedID)
convProg (Program exs body) = do
  mapM_ (addKnown . _name) exs
  Program exs <$> convExpr body

convExpr :: Expr TypedID -> Malgo LLEnv (Expr TypedID)
convExpr (Call fn args) = do
  table <- gets _table
  case Map.lookup fn table of
    (Just (fn', fv)) -> return $ Call fn' (fv ++ args)
    Nothing          -> return $ Call fn args
convExpr (Let decl e) = Let <$> convDecl decl <*> convExpr e
convExpr (If c t f) = If c <$> convExpr t <*> convExpr f
convExpr x = return x

convDecl :: Decl TypedID -> Malgo LLEnv (Decl TypedID)
convDecl (ValDec name val) = do
  -- addKnown name
  ValDec name <$> convExpr val
convDecl (FunDec fn params body) = do
  -- mapM_ addKnown (fn : params)
  k <- gets _knowns
  let fv = fvExpr body \\ ((fn : params) ++ k)
  case fv of
    []      -> FunDec fn params <$> convExpr body
    freevars -> do
      let (TypedID _ fnty) = fn
      let fnty' =
            FunTy (map (\(TypedID _ t) -> t) freevars
                    ++ _params fnty)
            $ _ret fnty
      let fn' = (\(TypedID n _) -> TypedID n fnty') fn
      modify $ \e -> e {
        _table = Map.insert fn (fn', freevars) (_table e)
        }
      FunDec fn' (freevars ++ params) <$> convExpr body
