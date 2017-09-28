module Language.Malgo.IR where

import           Control.Monad.State
import           Language.Malgo.Syntax (Name, Type)
import qualified Language.Malgo.Syntax as S

data Inst = Int Integer
          | Float Double
          | Bool Bool
          | Char Char
          | String String
          | Unit
          | Binary Op Id Id
          | If Id Block Block
          | App Id [Id] -- 関数適用
          | Appx Id [Id] -- 外部関数適用
          | Ref Id -- 変数参照
          | Refx Id -- 外部変数参照
          | Def Name [Inst]
          | Defun Name Int [Inst]
  deriving (Eq, Show)

type Block = [Inst]

data Op = Add | Sub | Mul | Div | Eq | Lt | Gt | And | Or
  deriving (Eq, Show)

type Id = String

data Env = Env { _k_id  :: Int -- K正規化に使用する整数
               , _names :: [Name]
               }

getK :: State Env String
getK = do
  env <- get
  return $ "$k" ++ show (_k_id env)

incK :: State Env ()
incK = do
  env <- get
  put $ env { _k_id = (_k_id env) + 1 }

getA :: String -> State Env String
getA name = do
  env <- get
  return $ name ++ "$a" ++ show (_a_id env)

incA :: State Env ()
incA = do
  env <- get
  put $ env { _a_id = (_a_id env) + 1 }

-- transExpr :: S.Expr -> [Inst]
transExpr (S.Int x)    = incK >> return [Int x]
transExpr (S.Float x)  = incK >> return [Float x]
transExpr (S.Bool x)   = incK >> return [Bool x]
transExpr (S.Char x)   = incK >> return [Char x]
transExpr (S.String x) = incK >> return [String x]
transExpr S.Unit       = incK >> return [Unit]
transExpr (S.Add x y) = do
  x' <- transExpr x
  xi <- getK
  y' <- transExpr y
  yi <- getK
  return $ x' ++ y' ++ [Binary Add xi yi]
transExpr (S.Var x) = do
  incK
  x' <- getA x
  return (Ref x')
transExpr _ = error "未実装"
