module Language.Malgo.Eval where

import           Control.Monad.State
import           Data.Either
import qualified Data.Map              as Map
import           Data.Maybe
import           Language.Malgo.Syntax (Name)
import qualified Language.Malgo.Syntax as S

data AST = Symbol Name
         | Int Integer
         | Float Double
         | Bool Bool
         | Char Char
         | String String
         | Typed AST AST
         | List [AST]
         | Tree [AST]
         | Proc [Name] AST Env
  deriving (Eq, Show)

type Env = Map.Map Name AST

trans :: S.AST -> AST
trans (S.Symbol s)  = Symbol s
trans (S.Int i)     = Int i
trans (S.Float f)   = Float f
trans (S.Bool b)    = Bool b
trans (S.Char c)    = Char c
trans (S.String s)  = String s
trans (S.Typed e t) = Typed (trans e) (trans t)
trans (S.List xs)   = List (map trans xs)
trans (S.Tree xs)   = Tree (map trans xs)

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
       _      -> lift . Left $ "error: " ++ show c ++ " is not Bool"

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

valueOf (Tree [Symbol "destruct", Tree symbols, List list, body]) =
  do extendEnv' symbols list
     valueOf body
     where extendEnv' ((Symbol name):srest) (val:vrest) =
             do val' <- valueOf val
                env <- get
                put $ extendEnv name val' env
                extendEnv' srest vrest
           extendEnv' [] [] = return ()
           extendEnv' x y = lift . Left $ "error: cannot destruct " ++ show x ++ " and " ++ show y

valueOf (Tree [Symbol "cond", Tree clauses]) = valueOfCond clauses

valueOf (Tree [Symbol "proc", Tree symbols, body]) =
  do env <- get
     let symbols' = catMaybes (map unSymbol symbols)
     if (length symbols') == (length symbols)
       then return $ Proc symbols' body env
       else lift . Left $ "error: " ++ show symbols' ++ " are not [Symbol a]"
  where unSymbol (Symbol s) = Just s
        unSymbol _          = Nothing

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
       _          -> lift . Left $ "error: " ++ show c ++ " is not Bool"
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

applyFun name args =
  do env <- get
     if Map.member name env
       then do Proc params body penv <- valueOf (Symbol name)
               put $ extendEnv' params args penv
               body' <- valueOf body
               put env
               return body'
       else lift . Left $ "error: call " ++ name ++ " with " ++ show args ++ "is invalid"
       where extendEnv' (name:nrest) (val:vrest) env = extendEnv' nrest vrest (extendEnv name val env)
             extendEnv' [] [] env = env

eval :: Env -> AST -> Either String (AST, Env)
eval env ast = runStateT (valueOf ast) env
