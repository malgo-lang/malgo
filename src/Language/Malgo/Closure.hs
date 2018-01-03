module Language.Malgo.Closure (conv) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map.Strict          as Map
import           Data.String
import qualified Language.Malgo.HIR       as H
import           Language.Malgo.MIR
import           Language.Malgo.Rename    (ID (..))
import           Language.Malgo.Syntax    (Type (..))
import           Language.Malgo.TypeCheck (TypedID (..))
import           Language.Malgo.Utils
import           Text.PrettyPrint

data ClsEnv = ClsEnv { _closures    :: Map.Map TypedID TypedID
                     , _knowns      :: [TypedID]
                     , _revToplevel :: [Decl TypedID]
                     , _revMain     :: [Instr TypedID]
                     , _count       :: Int
                     }
  deriving Show

initClsEnv :: ClsEnv
initClsEnv = ClsEnv Map.empty [] [] [] 0

type Closure a = Malgo ClsEnv a

runClosure :: Closure a -> (Either MalgoError a, ClsEnv)
runClosure m = runMalgo m initClsEnv

conv :: H.Program TypedID -> (Either MalgoError (Program TypedID), ClsEnv)
conv x = runClosure (convProgram x)

addKnown :: TypedID -> Closure ()
addKnown name =
  modify $ \e -> e { _knowns = name : _knowns e }

addToplevel :: Decl TypedID -> Closure ()
addToplevel tp =
  modify $ \e -> e { _revToplevel = tp : _revToplevel e }

addClosure :: TypedID -> TypedID -> Closure ()
addClosure orig cls =
  modify $ \e -> e { _closures = Map.insert orig cls (_closures e) }

newClsID :: TypedID -> [TypedID] -> Closure TypedID
newClsID (TypedID fn (FunTy params ret)) fv = do
  let ty = ClsTy params ret (map _type fv)
  c <- gets _count
  modify $ \e -> e { _count = c + 1 }
  return (TypedID
           (Internal (_name fn `mappend` fromString "$cls") c)
           ty)

convProgram :: H.Program TypedID
     -> Closure (Program TypedID)
convProgram (H.Program exs _ body) = do
  mapM_ (addKnown . H._name) exs
  convExterns exs
  convMain body
  t <- gets _revToplevel
  m <- gets _revMain
  return $ Program (reverse t) (reverse m)

convExterns :: [H.Extern TypedID] -> Closure ()
convExterns = mapM_ convExtern

convExtern :: H.Extern TypedID -> Closure ()
convExtern (H.ExDec name actual) =
  addToplevel $ ExDec name actual

convMain = undefined

convLet = undefined

convExpr :: H.Expr TypedID -> Closure (Expr TypedID)
convExpr (H.Call fn args) = do
  closures <- gets _closures
  case Map.lookup fn closures of
    (Just fn') -> return $ CallCls fn' args
    Nothing    -> return $ CallDir (_name . _id $ fn) args
convExpr (H.If c t f) = If c <$> convExpr t <*> convExpr f
convExpr e@H.Let{} = error $ "unreachable: " ++ show e
convExpr (H.Var x) = undefined
convExpr x = undefined

-- add :: TypedID -> ClsID -> Closure ()
-- add x x' =
--   modify $ \e -> e { _knowns = Map.insert x x' (_knowns e) }

-- convDecl :: Decl TypedID -> Closure (Decl ClsID)
-- convDecl = undefined

-- convExpr :: Expr TypedID -> Closure (Expr ClsID)
-- convExpr = undefined
-- convProgram = undefined
