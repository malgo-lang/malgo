module Language.Malgo.Rename where

import           Language.Malgo.Syntax
import           Language.Malgo.Utils
import           Prelude               hiding (lookup)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as Map

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.State

data RenamerState = RenamerState { _env   :: Map.Map Name Name
                                 , _count :: Int
                                 }
  deriving Show

type Renamer a = Malgo RenamerState a

runRenamer :: Renamer a -> (Either MalgoError a, RenamerState)
runRenamer m = runMalgo m (RenamerState Map.empty 0)

lookup :: Name -> Renamer (Maybe Name)
lookup name = do
  env <- gets _env
  return $ Map.lookup name env

rename :: Expr -> (Either MalgoError Expr, RenamerState)
rename e = runRenamer (renameExpr False e)

renameExpr :: Bool -> Expr -> Renamer Expr
renameExpr beRenamed (Let info decls expr) =
  Let info <$> mapM (renameDecl beRenamed) decls <*> renameExpr beRenamed expr
renameExpr _ (Var info name) = do
  mname <- lookup name
  case mname of
    Just x  -> return $ Var info x
    Nothing -> undefined

renameDecl :: Bool -> Decl -> Renamer Decl
renameDecl _ (FunDec info name params retty body) = undefined
