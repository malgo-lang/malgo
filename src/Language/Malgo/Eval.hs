{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Eval (eval) where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.Char
import           Data.List
import qualified Data.Map                 as Map
import           Language.Malgo.HIR       (Op (..))
import qualified Language.Malgo.MIR       as M
import           Language.Malgo.TypeCheck (TypedID (..))
import           Language.Malgo.Utils
import           System.IO
import           Text.PrettyPrint

data Value = Int Integer
           | Float Double
           | Bool Bool
           | Char Char
           | String String
           | Unit
           | Closure TypedID [TypedID]
  deriving (Show, Eq)

instance PrettyPrint Value where
  pretty (Int x)         = int (fromInteger x)
  pretty (Float x)       = double x
  pretty (Bool True)     = text "#t"
  pretty (Bool False)    = text "#f"
  pretty (Char x)        = quotes $ char x
  pretty (String x)      = doubleQuotes $ text x
  pretty Unit            = text "()"
  pretty (Closure fn fv) = braces $ pretty fn <+> sep (map pretty fv)

data Context = Context
  { _var :: Map.Map TypedID Value
  , _fun :: Map.Map TypedID ([Value] -> [Value] -> Eval Value)
  }

initContext :: Context
initContext = Context Map.empty Map.empty

prelude :: [(String, [Value] -> [Value] -> Eval Value)]
prelude =
  [ ( "print"
    , \[String x] [] -> lift (putStr x) >> return Unit)
  , ( "println"
    , \[String x] [] -> lift (putStrLn x) >> return Unit)
  , ( "print_int"
    , \[Int x] [] -> lift (print x) >> return Unit)
  , ( "print_float"
    , \[Float x] [] -> lift (print x) >> return Unit)
  , ( "flush"
    , \[Unit] [] -> lift (hFlush stdout) >> return Unit)
  , ( "getchar"
    , \[Unit] [] -> Char <$> lift getChar)
  , ( "ord"
    , \[Char x] [] -> return $ Int (toInteger $ ord x))
  , ( "chr"
    , \[Int x] [] -> return $ Char (chr $ fromInteger x))
  , ( "size"
    , \[String x] [] -> return $ Int (toInteger $ length x))
  , ( "not"
    , \[Bool x] [] -> return $ Bool (not x))
  ]

type Eval a = MalgoT Context IO a

runEval :: Eval a -> IO (Either MalgoError a, Context)
runEval m = runMalgoT m initContext

addVar :: TypedID -> Value -> Eval ()
addVar name val =
  modify $ \e -> e { _var = Map.insert name val (_var e) }

getVar :: TypedID -> Eval Value
getVar name = do
  vt <- gets _var
  case Map.lookup name vt of
    Nothing -> throw $ pretty name <+> text "is not defined"
    Just x  -> return x

addFun :: TypedID -> ([Value] -> [Value] -> Eval Value) -> Eval ()
addFun name fun =
  modify $ \e -> e { _fun = Map.insert name fun (_fun e) }

getFun :: TypedID -> Eval ([Value] -> [Value] -> Eval Value)
getFun name = do
  ft <- gets _fun
  case Map.lookup name ft of
    Nothing -> throw $ pretty name <+> text "is not defined"
    Just x  -> return x

eval :: M.Program TypedID -> IO (Either MalgoError Value, Context)
eval (M.Program tp ex e) = runEval $ do
  evalExtern ex
  evalToplevel tp
  evalExpr e

throw :: Doc -> Eval a
throw mes = throwError (EvalError mes)

evalToplevel :: [M.FunDec TypedID] -> Eval ()
evalToplevel [] = return ()
evalToplevel (M.FunDec name params capture body : xs) = do
  let fun ps cs = do
        backup <- gets _var
        mapM_ (uncurry addVar) (zip params ps)
        mapM_ (uncurry addVar) (zip capture cs)
        v <- evalExpr body
        modify $ \e -> e { _var = backup }
        return v
  addFun name fun
  evalToplevel xs

evalExtern :: [M.ExDec TypedID] -> Eval ()
evalExtern [] = return ()
evalExtern (M.ExDec name orig : xs) = do
  case lookup orig prelude of
    Nothing ->
      throw $ pretty name <+> text "is not found in prelude"
    Just x  -> addFun name x
  evalExtern xs

evalExpr :: M.Expr TypedID -> Eval Value
evalExpr (M.Var x)    = getVar x
evalExpr (M.Int x)    = return (Int x)
evalExpr (M.Float x)  = return (Float x)
evalExpr (M.Bool x)   = return (Bool x)
evalExpr (M.Char x)   = return (Char x)
evalExpr (M.String x) = return (String x)
evalExpr M.Unit       = return Unit
evalExpr (M.CallDir fn args) = do
  fn' <- getFun fn
  args' <- mapM getVar args
  fn' args' []
evalExpr (M.CallCls cls args) = do
  Closure fn capture <- getVar cls
  fn' <- getFun fn
  capture' <- mapM getVar capture
  args' <- mapM getVar args
  fn' args' capture'
evalExpr (M.Let (M.ValDec name val) body) = do
  val' <- evalExpr val
  addVar name val'
  evalExpr body
evalExpr (M.Let (M.ClsDec name fn fv) body) = do
  addVar name (Closure fn fv)
  evalExpr body
evalExpr (M.If c t f) = do
  c' <- getVar c
  case c' of
    Bool True  -> evalExpr t
    Bool False -> evalExpr f
    _          -> throw $ pretty c' <+> text "is not boolean"
evalExpr (M.BinOp op x y) = do
  x' <- getVar x
  y' <- getVar y
  case (op, x', y') of
    (Add, Int a, Int b)               -> return (Int $ a + b)
    (Sub, Int a, Int b)               -> return (Int $ a - b)
    (Mul, Int a, Int b)               -> return (Int $ a * b)
    (Div, Int a, Int b)               -> return (Int $ a `div` b)
    (Mod, Int a, Int b)               -> return (Int $ a `mod` b)
    (FAdd, Float a, Float b)          -> return (Float $ a + b)
    (FSub, Float a, Float b)          -> return (Float $ a - b)
    (FMul, Float a, Float b)          -> return (Float $ a * b)
    (FDiv, Float a, Float b)          -> return (Float $ a / b)
    (Eq _, a, b)                      -> return (Bool $ a == b)
    (Neq _, a, b)                     -> return (Bool $ a /= b)
    (Lt "Int", Int a, Int b)          -> return (Bool $ a < b)
    (Lt "Float", Float a, Float b)    -> return (Bool $ a < b)
    (Lt "Bool", Bool a, Bool b)       -> return (Bool $ a < b)
    (Lt "Char", Char a, Char b)       -> return (Bool $ a < b)
    (Lt "String", String a, String b) -> return (Bool $ a < b)
    (Gt "Int", Int a, Int b)          -> return (Bool $ a > b)
    (Gt "Float", Float a, Float b)    -> return (Bool $ a > b)
    (Gt "Bool", Bool a, Bool b)       -> return (Bool $ a > b)
    (Gt "Char", Char a, Char b)       -> return (Bool $ a > b)
    (Gt "String", String a, String b) -> return (Bool $ a > b)
    (Le "Int", Int a, Int b)          -> return (Bool $ a <= b)
    (Le "Float", Float a, Float b)    -> return (Bool $ a <= b)
    (Le "Bool", Bool a, Bool b)       -> return (Bool $ a <= b)
    (Le "Char", Char a, Char b)       -> return (Bool $ a <= b)
    (Le "String", String a, String b) -> return (Bool $ a <= b)
    (Ge "Int", Int a, Int b)          -> return (Bool $ a >= b)
    (Ge "Float", Float a, Float b)    -> return (Bool $ a >= b)
    (Ge "Bool", Bool a, Bool b)       -> return (Bool $ a >= b)
    (Ge "Char", Char a, Char b)       -> return (Bool $ a >= b)
    (Ge "String", String a, String b) -> return (Bool $ a >= b)
    (And, Bool a, Bool b)             -> return (Bool $ a && b)
    (Or, Bool a, Bool b)              -> return (Bool $ a || b)
    _ -> throw $ text (show (op, x', y') ++ " is not valid")
