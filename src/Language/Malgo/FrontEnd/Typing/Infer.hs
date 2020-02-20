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
    (_, subst) <- runWriterT $ typingExpr e
    env        <- gets (apply subst)

    opt        <- liftMalgo $ asks maOption
    when (dumpTypeTable opt) $ liftMalgo $ dump (toList env)

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

typingExpr :: (MonadWriter Subst m, MonadState Env m, MonadUniq m, MonadFail m)
           => Expr (ID ())
           -> m Type
typingExpr e = do
  (t, subst) <- listen $ typingExpr' e
  pure $ apply subst t

typingExpr' :: (MonadWriter Subst f, MonadState Env f, MonadUniq f, MonadFail f)
            => Expr (ID ())
            -> f Type
typingExpr' (Var _ x)    = lookupVar x
typingExpr' Int{}        = pure $ TyApp IntC []
typingExpr' Float{}      = pure $ TyApp FloatC []
typingExpr' Bool{}       = pure $ TyApp BoolC []
typingExpr' Char{}       = pure $ TyApp CharC []
typingExpr' String{}     = pure $ TyApp StringC []
typingExpr' (Tuple _ xs) = do
  ts <- mapM typingExpr xs
  pure $ TyApp TupleC ts
typingExpr' (Array i xs) = do
  ts <- mapM typingExpr xs
  ty <- newTyMeta
  updateSubst i "array literal" (toList $ fmap (ty :~) ts)
  pure $ TyApp ArrayC [ty]
typingExpr' (MakeArray i initNode sizeNode) = do
  initTy <- typingExpr initNode
  sizeTy <- typingExpr sizeNode
  updateSubst i "make array" [TyApp IntC [] :~ sizeTy]
  pure $ TyApp ArrayC [initTy]
typingExpr' (ArrayRead i arr _) = do
  arrTy    <- typingExpr arr
  resultTy <- newTyMeta
  updateSubst i "array read" [TyApp ArrayC [resultTy] :~ arrTy]
  pure resultTy
typingExpr' (ArrayWrite i arr ix val) = do
  arrTy <- typingExpr arr
  ixTy  <- typingExpr ix
  valTy <- typingExpr val
  updateSubst i "array write" [ixTy :~ TyApp IntC [], arrTy :~ TyApp ArrayC [valTy]]
  pure $ TyApp TupleC []
typingExpr' (Call i fn args) = do
  fnTy     <- typingExpr fn
  argTypes <- mapM typingExpr args
  retTy    <- newTyMeta
  updateSubst i "call" [TyApp FunC (retTy : argTypes) :~ fnTy]
  pure retTy
typingExpr' (Fn _ params body) = do
  paramTypes <- mapM (\(_, paramType) -> paramType `whenNothing` newTyMeta) params
  mapM_ (\((p, _), t) -> defineVar p $ Forall [] t) (zip params paramTypes)
  t <- typingExpr body
  pure $ TyApp FunC (t : paramTypes)
typingExpr' (Seq _ e1                       e2  ) = typingExpr e1 >> typingExpr e2
typingExpr' (Let _ (ValDec i name mtyp val) body) = do
  env     <- get
  valType <- typingExpr val

  case mtyp of
    Just typ -> updateSubst i "val signature" [valType :~ typ]
    Nothing  -> pure ()

  -- value restriction
  if isValue val then letVar env name valType else defineVar name (Forall [] valType)

  typingExpr body
typingExpr' (Let _ (ExDec _ name typ _) body) = do
  defineVar name (Forall [] typ)
  typingExpr body
typingExpr' (Let _ (FunDec fs) e) = do
  env <- get
  mapM_ prepare            fs
  mapM_ (typingFunDec env) fs
  typingExpr e
typingExpr' (If i c t f) = do
  ct <- typingExpr c
  tt <- typingExpr t
  ft <- typingExpr f
  updateSubst i "if" [ct :~ TyApp BoolC [], tt :~ ft]
  pure ft
typingExpr' (BinOp i op x y) = do
  opType     <- typingOp op
  xt         <- typingExpr x
  yt         <- typingExpr y
  resultType <- newTyMeta
  updateSubst i "binary op" [opType :~ TyApp FunC [resultType, xt, yt]]
  pure resultType
 where
  typingOp Add  = pure $ TyApp FunC [TyApp IntC [], TyApp IntC [], TyApp IntC []]
  typingOp Sub  = pure $ TyApp FunC [TyApp IntC [], TyApp IntC [], TyApp IntC []]
  typingOp Mul  = pure $ TyApp FunC [TyApp IntC [], TyApp IntC [], TyApp IntC []]
  typingOp Div  = pure $ TyApp FunC [TyApp IntC [], TyApp IntC [], TyApp IntC []]
  typingOp Mod  = pure $ TyApp FunC [TyApp IntC [], TyApp IntC [], TyApp IntC []]
  typingOp FAdd = pure $ TyApp FunC [TyApp FloatC [], TyApp FloatC [], TyApp FloatC []]
  typingOp FSub = pure $ TyApp FunC [TyApp FloatC [], TyApp FloatC [], TyApp FloatC []]
  typingOp FMul = pure $ TyApp FunC [TyApp FloatC [], TyApp FloatC [], TyApp FloatC []]
  typingOp FDiv = pure $ TyApp FunC [TyApp FloatC [], TyApp FloatC [], TyApp FloatC []]
  typingOp Eq   = newTyMeta >>= \a -> pure $ TyApp FunC [TyApp BoolC [], a, a]
  typingOp Neq  = newTyMeta >>= \a -> pure $ TyApp FunC [TyApp BoolC [], a, a]
  typingOp Lt   = newTyMeta >>= \a -> pure $ TyApp FunC [TyApp BoolC [], a, a]
  typingOp Gt   = newTyMeta >>= \a -> pure $ TyApp FunC [TyApp BoolC [], a, a]
  typingOp Le   = newTyMeta >>= \a -> pure $ TyApp FunC [TyApp BoolC [], a, a]
  typingOp Ge   = newTyMeta >>= \a -> pure $ TyApp FunC [TyApp BoolC [], a, a]
  typingOp And  = pure $ TyApp FunC [TyApp BoolC [], TyApp BoolC [], TyApp BoolC []]
  typingOp Or   = pure $ TyApp FunC [TyApp BoolC [], TyApp BoolC [], TyApp BoolC []]
typingExpr' (Match i scrutinee clauses) = do
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
