module Language.Malgo.LetLang where

import           Control.Monad.State
import           Data.Either
import qualified Data.Map              as Map
import           Language.Malgo.Syntax

type Env = Map.Map Name AST

initEnv :: Env
initEnv = Map.fromList [("nil", List [])]

extendEnv :: Name -> AST -> Env -> Env
extendEnv = Map.insert

applyEnv :: Env -> Name -> Maybe AST
applyEnv env var = Map.lookup var env

valueOf :: AST -> StateT Env (Either String) AST
valueOf (Tree [Symbol "if", c, t, e]) =
  -- (if c t e) -> if c then t else e
  do b <- valueOf c
     case b of
       Bool b -> if b then valueOf t else valueOf e
       _      -> lift . Left $ "error: " ++ textAST c ++ " is not Bool"

valueOf (Tree [Symbol "let", Symbol var, val, body]) =
  -- (let var val body) -> [var = val]body
  do val' <- valueOf val
     env <- get
     put $ extendEnv var val' env
     valueOf body

valueOf (Tree [Symbol "let*", Tree declist, body]) =
  do env <- get
     extendEnv' declist
     valueOf body
     where extendEnv' (Symbol var:val:rest) =
             do val' <- valueOf val
                env <- get
                put $ extendEnv var val' env
                extendEnv' rest
           extendEnv' [] = return ()

valueOf (Tree [Symbol "cond", Tree clauses]) = valueOfCond clauses
valueOf (Tree (Symbol fun : args)) =
  do env <- get
     let args' = map (eval env) args
     if null (lefts args')
       then applyFun fun (map fst (rights args'))
       else lift . Left $ head (lefts args')

valueOf (Symbol a) =
  do env <- get
     case applyEnv env a of
       Just ast -> return ast
       Nothing  -> lift $ Left ("error: " ++ a ++ " is not found")

valueOf x = return x

valueOfCond :: [AST] -> StateT Env (Either String) AST
valueOfCond (Tree [c, body] : rest) =
  do b <- valueOf c
     case b of
       Bool True  -> valueOf body
       Bool False -> valueOfCond rest
       _          -> lift . Left $ "error: " ++ textAST c ++ " is not Bool"
valueOfCond _ = lift . Left $ "error: cannot eval `cond`"

applyFun :: Name -> [AST] -> StateT Env (Either String) AST
applyFun "id" [a] = return a
applyFun "cons" [car, List cdr] = return $ List $ car:cdr
applyFun "car" [List (car:_)] = return car
applyFun "cdr" [List (_:cdr)] = return (List cdr)
applyFun "null?" [a] = return (Bool (a == List []))

applyFun "+" [Int lhs, Int rhs] = return (Int (lhs + rhs))
applyFun "-" [Int lhs, Int rhs] = return (Int (lhs - rhs))
applyFun "*" [Int lhs, Int rhs] = return (Int (lhs * rhs))
applyFun "/" [Int lhs, Int rhs] = return (Int (lhs `div` rhs))

applyFun "zero?" [a] = return (Bool (a == Int 0))
applyFun "minus" [Int i] = return (Int (-i))
applyFun name args = lift . Left $ "error: call " ++ name ++ " with " ++ show args ++ "is invalid"

eval :: Env -> AST -> Either String (AST, Env)
eval env ast = runStateT (valueOf ast) env
