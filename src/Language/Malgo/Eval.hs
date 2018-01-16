module Language.Malgo.Eval (eval) where

import           Control.Monad.Trans
import           Data.Char
import qualified Data.Map             as Map
import qualified Language.Malgo.MIR   as M
import           Language.Malgo.Utils
import           System.IO
import           Text.PrettyPrint

data Value a = Int Integer
             | Float Double
             | Bool Bool
             | Char Char
             | String String
             | Unit
             | Closure a [a]
  deriving (Show, Eq)

instance PrettyPrint a => PrettyPrint (Value a) where
  pretty (Int x)         = int (fromInteger x)
  pretty (Float x)       = double x
  pretty (Bool True)     = text "#t"
  pretty (Bool False)    = text "#f"
  pretty (Char x)        = quotes $ char x
  pretty (String x)      = doubleQuotes $ text x
  pretty Unit            = text "()"
  pretty (Closure fn fv) = braces $ pretty fn <+> sep (map pretty fv)

data Context a = Context
  { _var :: Map.Map a (Value a)
  , _fun :: Map.Map a ([Value a] -> [Value a] -> Eval (Value a))
  }

initContext :: Context a
initContext = Context Map.empty Map.empty

prelude :: [(String, [Value a] -> [Value a] -> Eval (Value a))]
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

type Eval a = MalgoT (Context a) IO a

runEval :: Eval a -> IO (Either MalgoError a, Context a)
runEval m = runMalgoT m initContext

eval :: M.Program a -> IO (Either MalgoError a, Context a)
eval = undefined

evalToplevel :: [M.FunDec a] -> Eval a
evalToplevel = undefined

evalMain :: M.Expr a -> Eval a
evalMain = undefined
