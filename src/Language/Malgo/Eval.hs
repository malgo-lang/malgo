module Language.Malgo.Eval
    ( eval
    , Value
    , Eval
    , Context
    ) where

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Data.Char
import qualified Data.Map                  as Map
import           Language.Malgo.HIR        (Op (..))
import qualified Language.Malgo.MIR        as M
import           Language.Malgo.TypeCheck  (TypedID (..))
import           Language.Malgo.Utils
import           System.IO
import           Text.PrettyPrint

data Value
    = Int Integer
    | Float Double
    | Bool Bool
    | Char Char
    | String String
    | Unit
    | Tuple [Value]
    | Closure TypedID
              [Value]
    deriving (Show, Eq, Ord)

instance PrettyPrint Value where
    pretty (Int x)         = int (fromInteger x)
    pretty (Float x)       = double x
    pretty (Bool True)     = text "#t"
    pretty (Bool False)    = text "#f"
    pretty (Char x)        = quotes $ char x
    pretty (String x)      = doubleQuotes $ text x
    pretty Unit            = text "()"
    pretty (Tuple xs) =
      braces $ sep (punctuate (text ",") (map pretty xs))
    pretty (Closure fn fv) = braces $ pretty fn <+> sep (map pretty fv)

data Context = Context
    { _var :: Map.Map TypedID Value
    , _fun :: Map.Map TypedID ([Value] -> [Value] -> Eval Value)
    , _gen :: Int
    }

instance Env Context where
    initEnv = Context Map.empty Map.empty
    updateUniq e i = e { _gen = i }
    getUniq = _gen

prelude :: [(String, [Value] -> [Value] -> Eval Value)]
prelude =
    [ ("print", \[String x] [] -> liftIO (putStr x) >> return Unit)
    , ("println", \[String x] [] -> liftIO (putStrLn x) >> return Unit)
    , ("print_int", \[Int x] [] -> liftIO (putStr $ show x) >> return Unit)
    , ("print_float", \[Float x] [] -> liftIO (putStr $ show x) >> return Unit)
    , ("newline", \[Unit] [] -> liftIO (putStrLn "") >> return Unit)
    , ("flush", \[Unit] [] -> liftIO (hFlush stdout) >> return Unit)
    , ("getchar", \[Unit] [] -> Char <$> liftIO getChar)
    , ("ord", \[Char x] [] -> return $ Int (toInteger $ ord x))
    , ("chr", \[Int x] [] -> return $ Char (chr $ fromInteger x))
    , ("size", \[String x] [] -> return $ Int (toInteger $ length x))
    , ("not", \[Bool x] [] -> return $ Bool (not x))
    ]

type Eval a = MalgoT Context IO a

addVar :: TypedID -> Value -> Eval ()
addVar name val = modify $ \e -> e {_var = Map.insert name val (_var e)}

getVar :: TypedID -> Eval Value
getVar name = do
    vt <- gets _var
    case Map.lookup name vt of
        Nothing -> throw $ pretty name <+> text "is not defined"
        Just x  -> return x

addFun :: TypedID -> ([Value] -> [Value] -> Eval Value) -> Eval ()
addFun name fun = modify $ \e -> e {_fun = Map.insert name fun (_fun e)}

getFun :: TypedID -> Eval ([Value] -> [Value] -> Eval Value)
getFun name = do
    ft <- gets _fun
    case Map.lookup name ft of
        Nothing -> throw $ pretty name <+> text "is not defined"
        Just x  -> return x

eval :: M.Program TypedID -> Eval Value
eval (M.Program tp ex e) = do
    evalExtern ex
    evalToplevel tp
    evalExpr e

throw :: Doc -> Eval a
throw mes = throwError (EvalError mes)

evalToplevel :: [M.FunDec TypedID] -> Eval ()
evalToplevel [] = return ()
evalToplevel (M.FunDec name params capture body:xs) = do
    let fun ps cs = do
            backup <- gets _var
            mapM_ (uncurry addVar) (zip params ps)
            mapM_ (uncurry addVar) (zip capture cs)
            v <- evalExpr body
            modify $ \e -> e {_var = backup}
            return v
    addFun name fun
    evalToplevel xs

evalExtern :: [M.ExDec TypedID] -> Eval ()
evalExtern [] = return ()
evalExtern (M.ExDec name orig:xs) = do
    case lookup orig prelude of
        Nothing -> throw $ pretty name <+> text "is not found in prelude"
        Just x  -> addFun name x
    evalExtern xs

evalExpr :: M.Expr TypedID -> Eval Value
evalExpr (M.Var x) = getVar x
evalExpr (M.Int x) = return (Int x)
evalExpr (M.Float x) = return (Float x)
evalExpr (M.Bool x) = return (Bool x)
evalExpr (M.Char x) = return (Char x)
evalExpr (M.String x) = return (String x)
evalExpr M.Unit = return Unit
evalExpr (M.Tuple xs) = Tuple <$> mapM getVar xs
evalExpr (M.TupleAccess x i) = do
  Tuple xs <- getVar x
  return $ xs !! i
evalExpr (M.CallDir fn args) = do
    fn' <- getFun fn
    args' <- mapM getVar args
    fn' args' []
evalExpr (M.CallCls cls args) = do
    Closure fn capture <- getVar cls
    fn' <- getFun fn
    args' <- mapM getVar args
    fn' args' capture
evalExpr (M.Let (M.ValDec name val) body) = do
    val' <- evalExpr val
    addVar name val'
    evalExpr body
evalExpr (M.Let (M.ClsDec name fn fv) body) = do
    capture <- mapM getVar fv
    addVar name (Closure fn capture)
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
        (Add, Int a, Int b) -> return (Int $ a + b)
        (Sub, Int a, Int b) -> return (Int $ a - b)
        (Mul, Int a, Int b) -> return (Int $ a * b)
        (Div, Int a, Int b) -> return (Int $ a `div` b)
        (Mod, Int a, Int b) -> return (Int $ a `mod` b)
        (FAdd, Float a, Float b) -> return (Float $ a + b)
        (FSub, Float a, Float b) -> return (Float $ a - b)
        (FMul, Float a, Float b) -> return (Float $ a * b)
        (FDiv, Float a, Float b) -> return (Float $ a / b)
        (Eq _, a, b) -> return (Bool $ a == b)
        (Neq _, a, b) -> return (Bool $ a /= b)
        (Lt _, a, b) -> return (Bool $ a < b)
        (Gt _, a, b) -> return (Bool $ a > b)
        (Le _, a, b) -> return (Bool $ a <= b)
        (Ge _, a, b) -> return (Bool $ a >= b)
        (And, Bool a, Bool b) -> return (Bool $ a && b)
        (Or, Bool a, Bool b) -> return (Bool $ a || b)
        _ -> throw $ text (show (op, x', y') ++ " is not valid")
