module Language.Malgo.Eval where

import           Control.Monad.State
import           Data.Either
import qualified Data.Map              as Map
import           Data.Maybe
import           Language.Malgo.Syntax

-- type Env = Map.Map Name AST

-- initEnv :: Env
-- initEnv = Map.fromList [("zero", Int 0)
--                        ,("id", Proc ["x"] (Symbol "x") Map.empty)]

-- extendEnv :: Name -> AST -> Env -> Env
-- extendEnv = Map.insert

-- applyEnv :: Env -> Name -> Maybe AST
-- applyEnv env var = Map.lookup var env

-- putEnv :: Name -> AST -> StateT Env (Either String) ()
-- putEnv var val =
--   do val' <- valueOf val
--      env <- get
--      put $ extendEnv var val' env
--      return ()

-- unSymbol (Symbol s) = Just s
-- unSymbol _          = Nothing

-- valueOf :: AST -> StateT Env (Either String) AST
-- valueOf (List [Symbol "if", c, t, e]) =
--   -- (if c t e) -> if c then t else e
--   do b <- valueOf c
--      case b of
--        Bool b -> if b then valueOf t else valueOf e
--        _      -> lift . Left $ "error: " ++ show c ++ " is not Bool"

-- valueOf (List [Symbol "let", Symbol var, val, body]) =
--   -- (let var val body) -> [var = val]body
--   do putEnv var val
--      valueOf body

-- valueOf (List [Symbol "let*", List declist, body]) =
--   do env <- get
--      extendEnv' declist
--      valueOf body
--      where extendEnv' (Symbol var:val:rest) =
--              do putEnv var val
--                 extendEnv' rest
--            extendEnv' [] = return ()

-- valueOf (List [Symbol "cond", List clauses]) = valueOfCond clauses

-- valueOf (List [Symbol "proc", List symbols, body]) =
--   do env <- get
--      let symbols' = mapMaybe unSymbol symbols
--      if length symbols' == length symbols
--        then return $ Proc symbols' body env
--        else lift . Left $ "error: " ++ show symbols' ++ " are not [Symbol a]"

-- valueOf (List (Symbol fun : args)) =
--   do env <- get
--      let args' = map (eval env) args
--      if null (lefts args')
--        then applyFun fun (map fst (rights args'))
--        else lift . Left $ head (lefts args')

-- valueOf (Symbol a) =
--   do env <- get
--      case applyEnv env a of
--        Just ast -> return ast
--        Nothing  -> lift $ Left ("error: " ++ a ++ " is not found")

-- valueOf x = return x

-- valueOfCond :: [AST] -> StateT Env (Either String) AST
-- valueOfCond (List [c, body] : rest) =
--   do b <- valueOf c
--      case b of
--        Bool True  -> valueOf body
--        Bool False -> valueOfCond rest
--        _          -> lift . Left $ "error: " ++ show c ++ " is not Bool"
-- valueOfCond _ = lift . Left $ "error: cannot eval `cond`"

-- applyFun :: Name -> [AST] -> StateT Env (Either String) AST
-- applyFun "+" [Int lhs, Int rhs] = return (Int (lhs + rhs))
-- applyFun "-" [Int lhs, Int rhs] = return (Int (lhs - rhs))
-- applyFun "*" [Int lhs, Int rhs] = return (Int (lhs * rhs))
-- applyFun "/" [Int lhs, Int rhs] = return (Int (lhs `div` rhs))

-- applyFun "zero?" [a] = return (Bool (a == Int 0))
-- applyFun "minus" [Int i] = return (Int (-i))

-- applyFun name args =
--   do env <- get
--      if Map.member name env
--        then do Proc params body penv <- valueOf (Symbol name)
--                if length params == length args
--                  then do put $ extendEnv' params args penv
-- {- Here is a dog -}      body' <- valueOf body
--                          put env
--                          return body'
--                  else lift . Left $ "error: " ++ "the number of args is invalid " ++ show params ++ ", " ++ show args
--        else lift . Left $ "error: call " ++ name ++ " with " ++ show args ++ "is invalid"
--        where extendEnv' (name:nrest) (val:vrest) env = extendEnv' nrest vrest (extendEnv name val env)
--              extendEnv' [] [] env = env

-- eval :: Env -> AST -> Either String (AST, Env)
-- eval env ast = runStateT (valueOf ast) env
