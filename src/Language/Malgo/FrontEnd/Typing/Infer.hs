{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Malgo.FrontEnd.Typing.Infer
  ( Typing
  , generalize
  )
where

import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Language.Malgo.IR.Syntax
                                         hiding ( info )

import           Language.Malgo.TypeRep.Type

import           Language.Malgo.FrontEnd.Info
import           Language.Malgo.FrontEnd.Typing.Constraint
import           Language.Malgo.FrontEnd.Typing.Subst

import           Relude.Unsafe                  ( fromJust )

data Typing

instance Pass Typing (Expr (ID ())) (Expr (ID Type)) where
  passName = "Typing"
  isDump   = dumpTyped
  trans e = evaluatingStateT mempty $ do
    _   <- typingExpr e
    env <- get

    opt <- getOpt
    when (dumpTypeTable opt) $ dump (toList env)

    pure $ fmap (\x -> removeExplictForall <$> fromJust (lookup x env)) e

type Env = IDMap () (ID Scheme)

newTyMeta :: MonadUniq m => m Type
newTyMeta = TyMeta <$> getUniq

generalize :: Env -> Type -> Scheme
generalize env t = Forall fv t where fv = toList $ ftv t \\ ftv env

letVar :: MonadState Env m => Env -> ID () -> Type -> m ()
letVar env var ty = do
  let sc = generalize env ty
  defineVar var sc

defineVar :: MonadState Env m => ID () -> Scheme -> m ()
defineVar x t = modify (insert x $ x { idMeta = t })

lookupVar :: (MonadFail f, MonadState Env f, MonadUniq f) => ID () -> f Type
lookupVar x = do
  Just ID { idMeta = scheme } <- lookup x <$> get
  instantiate scheme

updateSubst :: (MonadWriter Subst m, MonadState Env m) => Info -> Doc -> [Constraint] -> m ()
updateSubst i doc cs = do
  let subst = catchUnifyError i doc $ solve cs
  tell subst
  modify (apply subst)

applySubst :: (MonadState Env m, Substitutable a) => WriterT Subst m a -> m a
applySubst m = do
  (x, subst) <- runWriterT m
  modify (apply subst)
  pure (apply subst x)

typingExpr :: (MonadState Env f, MonadUniq f, MonadFail f) => Expr (ID ()) -> f Type
typingExpr (Var _ x)    = lookupVar x
typingExpr Int{}        = pure intTy
typingExpr Float{}      = pure floatTy
typingExpr Bool{}       = pure boolTy
typingExpr Char{}       = pure charTy
typingExpr String{}     = pure stringTy
typingExpr (Tuple _ xs) = applySubst $ tupleTy <$> mapM typingExpr xs
typingExpr (Array i xs) = applySubst $ do
  ts <- mapM typingExpr xs
  ty <- newTyMeta
  updateSubst i "array literal" (toList $ fmap (ty :~) ts)
  pure $ arrayTy ty
typingExpr (MakeArray i initNode sizeNode) = applySubst $ do
  initTy <- typingExpr initNode
  sizeTy <- typingExpr sizeNode
  updateSubst i "make array" [TyApp IntC [] :~ sizeTy]
  pure $ arrayTy initTy
typingExpr (ArrayRead i arr _) = applySubst $ do
  arrTy    <- typingExpr arr
  resultTy <- newTyMeta
  updateSubst i "array read" [TyApp ArrayC [resultTy] :~ arrTy]
  pure resultTy
typingExpr (ArrayWrite i arr ix val) = applySubst $ do
  arrTy <- typingExpr arr
  ixTy  <- typingExpr ix
  valTy <- typingExpr val
  updateSubst i "array write" [ixTy :~ TyApp IntC [], arrTy :~ TyApp ArrayC [valTy]]
  pure $ tupleTy []
typingExpr (Call i fn args) = applySubst $ do
  fnTy     <- typingExpr fn
  argTypes <- mapM typingExpr args
  retTy    <- newTyMeta
  updateSubst i "call" [TyApp FunC (retTy : argTypes) :~ fnTy]
  pure retTy
typingExpr (Fn _ params body) = applySubst $ do
  paramTypes <- mapM (\(_, paramType) -> paramType `whenNothing` newTyMeta) params
  zipWithM_ (\(p, _) t -> defineVar p $ Forall [] t) params paramTypes
  retType <- typingExpr body
  pure $ paramTypes --> retType
typingExpr (Seq _ e1                       e2  ) = applySubst $ typingExpr e1 >> typingExpr e2
typingExpr (Let _ (ValDec i name mtyp val) body) = applySubst $ do
  env     <- get
  valType <- typingExpr val

  case mtyp of
    Just typ -> updateSubst i "val signature" [valType :~ typ]
    Nothing  -> pure ()

  -- value restriction
  if isValue val then letVar env name valType else defineVar name (Forall [] valType)

  typingExpr body
typingExpr (Let _ (ExDec _ name typ _) body) = applySubst $ do
  defineVar name (Forall [] typ)
  typingExpr body
typingExpr (Let _ (FunDec fs) e) = applySubst $ do
  env <- get
  mapM_ prepare            fs
  mapM_ (typingFunDec env) fs
  typingExpr e
typingExpr (If i c t f) = applySubst $ do
  ct <- typingExpr c
  tt <- typingExpr t
  ft <- typingExpr f
  updateSubst i "if" [ct :~ boolTy, tt :~ ft]
  pure ft
typingExpr (BinOp i op x y) = applySubst $ do
  opType     <- typingOp op
  xt         <- typingExpr x
  yt         <- typingExpr y
  resultType <- newTyMeta
  updateSubst i "binary op" [opType :~ [xt, yt] --> resultType]
  pure resultType
 where
  typingOp Add  = pure $ [intTy, intTy] --> intTy
  typingOp Sub  = pure $ [intTy, intTy] --> intTy
  typingOp Mul  = pure $ [intTy, intTy] --> intTy
  typingOp Div  = pure $ [intTy, intTy] --> intTy
  typingOp Mod  = pure $ [intTy, intTy] --> intTy
  typingOp FAdd = pure $ [floatTy, floatTy] --> floatTy
  typingOp FSub = pure $ [floatTy, floatTy] --> floatTy
  typingOp FMul = pure $ [floatTy, floatTy] --> floatTy
  typingOp FDiv = pure $ [floatTy, floatTy] --> floatTy
  typingOp Eq   = newTyMeta >>= \a -> pure $ [a, a] --> boolTy
  typingOp Neq  = newTyMeta >>= \a -> pure $ [a, a] --> boolTy
  typingOp Lt   = newTyMeta >>= \a -> pure $ [a, a] --> boolTy
  typingOp Gt   = newTyMeta >>= \a -> pure $ [a, a] --> boolTy
  typingOp Le   = newTyMeta >>= \a -> pure $ [a, a] --> boolTy
  typingOp Ge   = newTyMeta >>= \a -> pure $ [a, a] --> boolTy
  typingOp And  = pure $ [boolTy, boolTy] --> boolTy
  typingOp Or   = pure $ [boolTy, boolTy] --> boolTy
typingExpr (Match i scrutinee clauses) = applySubst $ do
  ty1 <- typingExpr scrutinee
  let (pats, exprs) = unzip clauses
  mapM_ (typingPat i ty1) pats
  t :| _ <- mapM typingExpr exprs
  pure t

prepare :: (MonadState Env m, MonadUniq m) => (a, ID (), c, d, e) -> m ()
prepare (_, f, _, _, _) = defineVar f . Forall [] =<< newTyMeta

typingFunDec :: (MonadUniq m, MonadState Env m, MonadWriter Subst m, MonadFail m)
             => Env
             -> (Info, ID (), [(ID (), Maybe Type)], Maybe Type, Expr (ID ()))
             -> m ()
typingFunDec env (i', f, params, mretty, body) = do
  (tv, subst) <- listen $ do
    paramTypes <- mapM (\(_, paramType) -> paramType `whenNothing` newTyMeta) params
    retType    <- mretty `whenNothing` newTyMeta

    mapM_ (\((p, _), t) -> defineVar p $ Forall [] t) (zip params paramTypes)
    t  <- typingExpr body
    tv <- lookupVar f
    updateSubst i' "fun dec" [tv :~ TyApp FunC (retType : paramTypes), t :~ retType]
    pure tv
  letVar (apply subst env) f (apply subst tv)

typingPat :: (MonadState Env m, MonadUniq m, MonadWriter Subst m)
          => Info
          -> Type
          -> Pat (ID ())
          -> m ()
typingPat _ ty (VarP   x ) = defineVar x (Forall [] ty)
typingPat i ty (TupleP ps) = do
  vs <- replicateM (length ps) newTyMeta
  updateSubst i "tuple pat" [ty :~ TyApp TupleC vs]
  zipWithM_ (typingPat i) vs ps

isValue :: Expr a -> Bool
isValue Var{}        = True
isValue Int{}        = True
isValue Float{}      = True
isValue Bool{}       = True
isValue Char{}       = True
isValue String{}     = True
isValue Fn{}         = True
isValue (Tuple _ xs) = all isValue xs
isValue _            = False
