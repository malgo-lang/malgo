{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE MultiWayIf #-}

module Language.Malgo.Typing where

import           Control.Arrow         ((&&&))
import           Control.Monad.State
import           Data.Either           ()
import           Language.Malgo.HIR
import           Language.Malgo.Syntax

type Env = [(Name, Type)]

initEnv :: Env
initEnv = []

addBind :: Name -> Type -> StateT Env (Either String) ()
addBind n t = do
  ctx <- get
  put $ (n, t):ctx

getType :: Name -> Pos -> StateT Env (Either String) Type
getType n _ = do
  ctx <- get
  case lookup n ctx of
    Just ty -> return ty
    Nothing -> lift . Left $ "error: " ++ show n ++ " is not defined.\n"

typeEq :: Type -> Type -> Bool
typeEq = (==)

typeError :: String -> String -> String -> StateT Env (Either String) a
typeError expected actual info = lift . Left $ show info ++ " error: Expected -> " ++ expected ++ "; Actual -> " ++ actual

typedExpr :: Expr -> StateT Env (Either String) (EXPR 'Typed)
typedExpr (Var pos name) = do
  ty <- getType name pos
  return (VAR (name2Id name), ty)
typedExpr (Int _ x)    = return (INT x, IntTy)
typedExpr (Float _ x)  = return (FLOAT x, FloatTy)
typedExpr (Bool _ x)   = return (BOOL x, BoolTy)
typedExpr (Char _ x)   = return (CHAR x, CharTy)
typedExpr (String _ x) = return (STRING x, StringTy)
typedExpr (Unit _) = return (UNIT, UnitTy)

typedExpr (Call info name args) = do
  args' <- mapM typedExpr args
  let argsTy = map snd args'
  funty <- getType name info
  case funty of
    FunTy retTy paramsTy -> if and $ zipWith typeEq argsTy paramsTy
                            then return (CALL (name2Id name) args', retTy)
                            else typeError (show paramsTy) (show argsTy) (show info )
    _ -> typeError "Function" (show funty) (show info)

typedExpr (Seq _ (Let info2 name ty val) body) = do
  val' <- typedExpr val
  let vt = snd val'
  if typeEq ty vt
    then do
      addBind name ty
      body' <- typedExpr body
      return (LET (name2Id name) ty val' body', snd body')
    else typeError (show ty) (show vt) (show info2)

typedExpr (Seq info e1@Seq{} e2) = do
  ctx <- get
  e1' <- typedExpr e1
  let ty1 = snd e1'
  put ctx
  if typeEq ty1 UnitTy
    then do e2' <- typedExpr e2
            return (LET (name2Id "$_") UnitTy e1' e2', snd e2')
    else typeError (show UnitTy) (show ty1) (show info)

typedExpr (Seq info e1 e2) = do
  e1' <- typedExpr e1
  let ty1 = snd e1'
  if typeEq ty1 UnitTy
    then do e2' <- typedExpr e2
            return (LET (name2Id "$_") UnitTy e1' e2', snd e2')
    else typeError (show UnitTy) (show ty1) (show info)

typedExpr (If info c t f) = do
  c' <- typedExpr c
  t' <- typedExpr t
  f' <- typedExpr f
  let ct = snd c'
  let tt = snd t'
  let ft = snd f'

  if typeEq ct BoolTy
    then if typeEq tt ft
         then return (IF c' t' f', tt)
         else typeError (show tt) (show ft) (show info)
    else typeError (show BoolTy) (show ct) (show info)

typedExpr (BinOp info op e1 e2) = do
  e1' <- typedExpr e1
  e2' <- typedExpr e2
  let t1 = snd e1'
  let t2 = snd e2'
  if | op `elem` [Add, Sub, Mul, Div] ->
       if | typeEq t1 IntTy ->
              if typeEq t2 IntTy
              then return (BINOP op e1' e2', t2)
              else typeError (show IntTy) (show t2) (show info)
          | typeEq t1 FloatTy ->
              if typeEq t2 FloatTy
              then return (BINOP op e1' e2', t2)
              else typeError (show FloatTy) (show t2) (show info)
          | otherwise ->
              typeError (show IntTy ++ " or " ++ show FloatTy) (show t1) (show info)
     | op `elem` [Eq, Lt, Gt, Le, Ge] ->
       if | t1 `elem` comparableTypes ->
            if typeEq t1 t2
            then return (BINOP op e1' e2', BoolTy)
            else typeError (show t1) (show t2) (show info)
          | otherwise -> typeError ("any one of " ++ show comparableTypes) (show t1) (show info)
     | op `elem` [And, Or] ->
       if typeEq t1 BoolTy
       then if typeEq t2 BoolTy
            then return (BINOP op e1' e2', BoolTy)
            else typeError (show BoolTy) (show t2) (show info)
       else typeError (show BoolTy) (show t1) (show info)
  where comparableTypes = [IntTy, FloatTy, BoolTy, CharTy, StringTy]

typedExpr Let{} = return (UNIT, UnitTy)

isConstError info = lift . Left $ show info ++ " error: initializer element is not a compile-time element"
isConst :: Expr -> StateT Env (Either String) ()
isConst (Call info _ _)    = isConstError info
isConst (Seq info _ _)     = isConstError info
isConst (Let info _ _ _)   = isConstError info
isConst (If info _ _ _)    = isConstError info
isConst (BinOp info _ _ _) = isConstError info
isConst _                  = return ()

typedDecl :: Decl -> StateT Env (Either String) (DECL 'Typed)
typedDecl (Def info n ty val) = do
  isConst val
  val' <- typedExpr val
  let tv = snd val'
  if typeEq ty tv
    then addBind n ty >> return (DEF (name2Id n) ty val')
    else typeError (show ty) (show tv) (show info)
typedDecl (Defun info fn retTy params body) = do
  let funTy = FunTy retTy (map snd params)
  ctx <- get
  put $ params ++ ctx
  addBind fn funTy
  body' <- typedExpr body
  let bodyTy = snd body'
  put ctx
  if typeEq retTy bodyTy
    then do
      addBind fn funTy
      return (DEFUN (name2Id fn) retTy
              (map ((name2Id . fst) &&& snd) params)
              body')
    else typeError (show retTy) (show bodyTy) (show info)

typedDecl (ExDef _ n ty) = addBind n ty >> return (EXDEF (name2Id n) ty)
typedDecl (ExDefun _ fn retTy params) = do
  let funTy = FunTy retTy (map snd params)
  addBind fn funTy
  return (EXDEFUN (name2Id fn) retTy
          (map ((name2Id . fst) &&& snd) params))

typing :: Traversable f => f Decl -> Either String (f (HIR 'Typed))
typing ast = fmap HIR <$> evalStateT (mapM typedDecl ast) initEnv

testEnv :: Env
testEnv = [ (mkName "print", FunTy UnitTy [StringTy])
          , (mkName "println", FunTy UnitTy [StringTy])
          , (mkName "print_int", FunTy UnitTy [IntTy])
          ]

evalTypedExpr :: Expr -> Either String (EXPR 'Typed)
evalTypedExpr expr = evalStateT (typedExpr expr) testEnv

evalTypedDecl :: Decl -> Either String (DECL 'Typed)
evalTypedDecl decl = evalStateT (typedDecl decl) testEnv
