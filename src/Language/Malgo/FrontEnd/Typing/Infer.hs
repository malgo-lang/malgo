{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import qualified Language.Malgo.IR.Syntax      as Syntax

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
    (_, cs) <- runWriterT $ typingExpr e
    let subst = catchUnifyError (Syntax.info e) "toplevel" (solve cs)
    env <- gets (apply subst)

    opt <- liftMalgo $ asks maOption
    when (dumpTypeTable opt) $ do
      let xs = map (\x@ID { idMeta } -> (x, idMeta)) (toList env)
      liftMalgo $ dump xs

    pure $ fmap (\x -> removeExplictForall <$> fromJust (lookup x env)) e

type Env = IDMap () (ID Scheme)

newTyMeta :: MonadUniq m => m Type
newTyMeta = TyMeta <$> getUniq

generalize :: Env -> Type -> Scheme
generalize env t = Forall fv t where fv = toList $ ftv t \\ ftv env

letVar :: MonadState Env m => Info -> Env -> ID () -> Type -> [Constraint] -> m ()
letVar info env var ty cs = do
  let subst = catchUnifyError info (pPrint var) (solve cs)
  let sc    = generalize (apply subst env) (apply subst ty)
  defineVar var sc
  modify (apply subst)

defineVar :: MonadState Env m => ID () -> Scheme -> m ()
defineVar x t = modify (insert x $ x { idMeta = t })

lookupVar :: (MonadFail f, MonadState Env f, MonadUniq f) => ID () -> f Type
lookupVar x = do
  Just ID { idMeta = scheme } <- lookup x <$> get
  instantiate scheme

typingExpr :: (MonadWriter [Constraint] f, MonadState Env f, MonadUniq f, MonadFail f)
           => Expr (ID ())
           -> f Type
typingExpr (Var _ x)    = lookupVar x
typingExpr Int{}        = pure $ TyApp IntC []
typingExpr Float{}      = pure $ TyApp FloatC []
typingExpr Bool{}       = pure $ TyApp BoolC []
typingExpr Char{}       = pure $ TyApp CharC []
typingExpr String{}     = pure $ TyApp StringC []
typingExpr (Tuple _ xs) = do
  ts <- mapM typingExpr xs
  pure $ TyApp TupleC ts
typingExpr (Array _ xs) = do
  ts <- mapM typingExpr xs
  ty <- newTyMeta
  tell (toList $ fmap (ty :~) ts)
  pure $ TyApp ArrayC [ty]
typingExpr (MakeArray _ initNode sizeNode) = do
  initTy <- typingExpr initNode
  sizeTy <- typingExpr sizeNode
  tell [TyApp IntC [] :~ sizeTy]
  pure $ TyApp ArrayC [initTy]
typingExpr (ArrayRead _ arr _) = do
  arrTy    <- typingExpr arr
  resultTy <- newTyMeta
  tell [TyApp ArrayC [resultTy] :~ arrTy]
  pure resultTy
typingExpr (ArrayWrite _ arr ix val) = do
  arrTy <- typingExpr arr
  ixTy  <- typingExpr ix
  valTy <- typingExpr val
  tell [ixTy :~ TyApp IntC [], arrTy :~ TyApp ArrayC [valTy]]
  pure $ TyApp TupleC []
typingExpr (Call _ fn args) = do
  fnTy     <- typingExpr fn
  argTypes <- mapM typingExpr args
  retTy    <- newTyMeta
  tell [TyApp FunC (retTy : argTypes) :~ fnTy]
  pure retTy
typingExpr (Fn _ params body) = do
  paramTypes <- mapM (\(_, paramType) -> paramType `whenNothing` newTyMeta) params
  mapM_ (\((p, _), t) -> defineVar p $ Forall [] t) (zip params paramTypes)
  t <- typingExpr body
  pure $ TyApp FunC (t : paramTypes)
typingExpr (Seq i e1 e2) = do
  (_, cs1) <- listen $ typingExpr e1
  catchUnifyError i "seq" (solve cs1) `seq` typingExpr e2
typingExpr (Let _ (ValDec i name mtyp val) body) = do
  env            <- get

  (valType, cs1) <- listen $ typingExpr val

  cs2            <- case mtyp of
    Just typ -> tell [valType :~ typ] >> pure (valType :~ typ : cs1)
    Nothing  -> pure cs1

  -- value restriction
  if isValue val then letVar i env name valType cs2 else defineVar name (Forall [] valType)

  typingExpr body
typingExpr (Let _ (ExDec _ name typ _) body) = do
  defineVar name (Forall [] typ)
  typingExpr body
typingExpr (Let _ (FunDec fs) e) = do
  env <- get
  mapM_ prepare            fs
  mapM_ (typingFunDec env) fs
  typingExpr e
 where
  prepare (_, f, _, _, _) = defineVar f . Forall [] =<< newTyMeta
  typingFunDec env (i', f, params, mretty, body) = do
    paramTypes <- mapM (\(_, paramType) -> paramType `whenNothing` newTyMeta) params
    retType    <- mretty `whenNothing` newTyMeta

    mapM_ (\((p, _), t) -> defineVar p $ Forall [] t) (zip params paramTypes)
    (t, cs1) <- listen $ typingExpr body
    tv       <- lookupVar f
    let cs2 = [tv :~ TyApp FunC (retType : paramTypes), t :~ retType]
    tell cs2
    letVar i' env f tv $ cs2 <> cs1

typingExpr (If _ c t f) = do
  ct <- typingExpr c
  tt <- typingExpr t
  ft <- typingExpr f
  tell [ct :~ TyApp BoolC [], tt :~ ft]
  pure ft
typingExpr (BinOp _ op x y) = do
  opType     <- typingOp op
  xt         <- typingExpr x
  yt         <- typingExpr y
  resultType <- newTyMeta
  tell [opType :~ TyApp FunC [resultType, xt, yt]]
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
typingExpr (Match _ scrutinee clauses) = do
  ty1 <- typingExpr scrutinee
  let (pats, exprs) = unzip clauses
  mapM_ (typingPat ty1) pats
  t :| _ <- mapM typingExpr exprs
  pure t

typingPat :: (MonadState Env m, MonadUniq m, MonadWriter [Constraint] m)
          => Type
          -> Pat (ID ())
          -> m ()
typingPat ty (VarP   x ) = defineVar x (Forall [] ty)
typingPat ty (TupleP ps) = do
  vs <- replicateM (length ps) newTyMeta
  tell [ty :~ TyApp TupleC vs]
  zipWithM_ typingPat vs ps

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
