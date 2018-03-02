{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Language.Malgo.Eval
    ( eval
    , Value
    , Eval
    , Context
    ) where

import qualified Data.Text                as T
import           Language.Malgo.HIR       (Op (..))
import qualified Language.Malgo.MIR       as M
import           Language.Malgo.Prelude
import           Language.Malgo.TypeCheck (TypedID (..))
import           Language.Malgo.Utils
import qualified System.IO                as S
import           Text.PrettyPrint

data Value = Int Integer
           | Float Double
           | Bool Bool
           | Char Char
           | String Text
           | Unit
           | Tuple [Value]
           | Closure TypedID [Value]
    deriving (Show, Eq, Ord)

instance PrettyPrint Value where
    pretty (Int x)         = int (fromInteger x)
    pretty (Float x)       = double x
    pretty (Bool True)     = text "#t"
    pretty (Bool False)    = text "#f"
    pretty (Char x)        = quotes $ char x
    pretty (String x)      = doubleQuotes $ pretty x
    pretty Unit            = text "()"
    pretty (Tuple xs) =
      braces $ sep (punctuate (text ",") (map pretty xs))
    pretty (Closure fn fv) = braces $ pretty fn <+> sep (map pretty fv)

data Context = Context
    { _var :: Map TypedID Value
    , _fun :: Map TypedID ([Value] -> [Value] -> Eval Value)
    }

instance Env Context where
    initEnv = Context mempty mempty

prelude :: Map Text ([Value] -> [Value] -> Eval Value)
prelude = fromList
    [ ("print", \[String x] [] ->
          do liftIO (putStr x)
             pure Unit)
    , ("println", \[String x] [] ->
          do liftIO (putStrLn x)
             pure Unit)
    , ("print_int", \[Int x] [] ->
          do liftIO (putStr @Text $ show x)
             pure Unit)
    , ("print_float", \[Float x] [] ->
          do liftIO (putStr @Text $ show x)
             pure Unit)
    , ("newline", \[Unit] [] ->
          do liftIO (putStrLn @Text "")
             pure Unit)
    , ("flush", \[Unit] [] ->
          do liftIO (S.hFlush stdout)
             pure Unit)
    , ("getchar", \[Unit] [] -> Char <$> liftIO S.getChar)
    , ("ord", \[Char x] [] -> pure $ Int (toInteger $ ord x))
    , ("chr", \[Int x] [] -> pure $ Char (chr $ fromInteger x))
    , ("size", \[String x] [] -> pure $ Int (toInteger $ T.length x))
    , ("not", \[Bool x] [] -> pure $ Bool (not x))
    ]

type Eval a = MalgoT Context IO a

addVar :: TypedID -> Value -> Eval ()
addVar name val = modify $ \e -> e {_var = insert name val (_var e)}

getVar :: TypedID -> Eval Value
getVar name = do
  vt <- gets _var
  case lookup name vt of
    Nothing -> throw $ pretty name <+> text "is not defined"
    Just x  -> pure x

addFun :: TypedID -> ([Value] -> [Value] -> Eval Value) -> Eval ()
addFun name fun = modify $ \e -> e {_fun = insert name fun (_fun e)}

getFun :: TypedID -> Eval ([Value] -> [Value] -> Eval Value)
getFun name = do
  ft <- gets _fun
  case lookup name ft of
    Nothing -> throw $ pretty name <+> text "is not defined"
    Just x  -> pure x

eval :: M.Program TypedID -> Eval Value
eval (M.Program tp ex e _) = do
  evalExtern ex
  evalToplevel tp
  evalExpr e

throw :: Doc -> Eval a
throw mes = throwError (EvalError mes)

evalToplevel :: [M.FunDec TypedID] -> Eval ()
evalToplevel [] = pure ()
evalToplevel (M.FunDec name params capture body:xs) = do
  let fun ps cs = do
        backup <- gets _var
        mapM_ (uncurry addVar) (zip params ps)
        mapM_ (uncurry addVar) (zip capture cs)
        v <- evalExpr body
        modify $ \e -> e {_var = backup}
        pure v
  addFun name fun
  evalToplevel xs

evalExtern :: [M.ExDec TypedID] -> Eval ()
evalExtern [] = pure ()
evalExtern (M.ExDec name orig:xs) = do
  case lookup orig prelude of
    Nothing -> throw $ pretty name <+> text "is not found in prelude"
    Just x  -> addFun name x
  evalExtern xs

evalExpr :: M.Expr TypedID -> Eval Value
evalExpr (M.Var x) = getVar x
evalExpr (M.Int x) = pure (Int x)
evalExpr (M.Float x) = pure (Float x)
evalExpr (M.Bool x) = pure (Bool x)
evalExpr (M.Char x) = pure (Char x)
evalExpr (M.String x) = pure (String x)
evalExpr M.Unit = pure Unit
evalExpr (M.Tuple xs) = Tuple <$> mapM getVar xs
evalExpr (M.TupleAccess x i) = do
  Tuple xs <- getVar x
  pure $ fromMaybe (panic "out of bounds") (atMay xs i)
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
    (Add, Int a, Int b) -> pure (Int $ a + b)
    (Sub, Int a, Int b) -> pure (Int $ a - b)
    (Mul, Int a, Int b) -> pure (Int $ a * b)
    (Div, Int a, Int b) -> pure (Int $ a `div` b)
    (Mod, Int a, Int b) -> pure (Int $ a `mod` b)
    (FAdd, Float a, Float b) -> pure (Float $ a + b)
    (FSub, Float a, Float b) -> pure (Float $ a - b)
    (FMul, Float a, Float b) -> pure (Float $ a * b)
    (FDiv, Float a, Float b) -> pure (Float $ a / b)
    (Eq _, a, b) -> pure (Bool $ a == b)
    (Neq _, a, b) -> pure (Bool $ a /= b)
    (Lt _, a, b) -> pure (Bool $ a < b)
    (Gt _, a, b) -> pure (Bool $ a > b)
    (Le _, a, b) -> pure (Bool $ a <= b)
    (Ge _, a, b) -> pure (Bool $ a >= b)
    (And, Bool a, Bool b) -> pure (Bool $ a && b)
    (Or, Bool a, Bool b) -> pure (Bool $ a || b)
    _ -> throw $ text (show (op, x', y') ++ " is not valid")
