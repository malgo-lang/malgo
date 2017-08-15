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
  do b <- valueOf c
     case b of
       Bool b -> if b then valueOf t else valueOf e
       _      -> lift . Left $ textAST c ++ " is not Bool"

valueOf (Tree [Symbol "let", Symbol var, val, body]) =
  do val' <- valueOf val
     env <- get
     put $ extendEnv var val' env
     valueOf body

valueOf (Tree (Symbol fun : args)) =
  do env <- get
     let args' = map (`runValueOf` env) args
     if null (lefts args')
       then applyFun fun (map fst (rights args'))
       else lift . Left $ head (lefts args')

valueOf (Symbol a) =
  do env <- get
     case applyEnv env a of
       Just ast -> return ast
       Nothing  -> lift $ Left (a ++ " is not found")

valueOf x = return x


applyFun :: Name -> [AST] -> StateT Env (Either String) AST
applyFun "id" [a]    = return a
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
applyFun name args   = lift . Left $ "call " ++ name ++ " with " ++ show args ++ "is invalid"

runValueOf :: AST -> Env -> Either String (AST, Env)
runValueOf ast = runStateT (valueOf ast)
