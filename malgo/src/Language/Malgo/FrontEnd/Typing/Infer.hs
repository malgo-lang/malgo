{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.FrontEnd.Typing.Infer
  ( Typing,
    generalize,
  )
where

import Data.Set ((\\))
import Language.Malgo.FrontEnd.Typing.Constraint
import Language.Malgo.FrontEnd.Typing.Subst
import Language.Malgo.IR.Syntax
import Language.Malgo.Id
import Language.Malgo.Monad
import Language.Malgo.Pass
import Language.Malgo.Prelude hiding (ix)
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.SType
import Language.Malgo.TypeRep.Type
import Text.Parsec (SourcePos)

data Typing

instance Pass Typing (Expr (Id ())) (Expr (Id Type)) where
  passName = "Typing"
  trans e = evalStateT ?? mempty $ do
    (_, cs) <- runWriterT (typingExpr e)
    case solve cs of
      Right subst -> do
        modify (apply subst)
        env <- get
        opt <- getOpt
        when (dumpTypeTable opt) $ dump (toList env)
        pure $ fmap (\x -> maybe (x & idMeta .~ TyMeta (-1)) (fmap removeExplictForall) (env ^. at x)) e
      Left doc -> errorDoc doc

type Env = Map (Id ()) (Id Scheme)

newTyMeta :: MonadUniq m => m Type
newTyMeta = TyMeta <$> getUniq

generalize :: Env -> Type -> Scheme
generalize env t = Forall (toList $ ftv t \\ ftv env) t

letVar ::
  MonadState Env m =>
  Env ->
  Id () ->
  Type ->
  [WithPos] ->
  m ()
letVar env var ty cs =
  case solve cs of
    Right subst -> do
      defineVar var $ generalize (apply subst env) (apply subst ty)
      modify (apply subst)
    Left doc -> errorDoc doc

defineVar :: MonadState Env m => Id () -> Scheme -> m ()
defineVar x t = modify (at x ?~ (x & idMeta .~ t))

lookupVar :: (MonadState Env f, MonadUniq f) => Id () -> f Type
lookupVar x = do
  env <- get
  case env ^. at x of
    Just x' -> instantiate $ x' ^. idMeta
    Nothing -> bug Unreachable

addCs ::
  (MonadState Env m, MonadMalgo m) =>
  SourcePos ->
  [Constraint] ->
  WriterT [WithPos] m ()
addCs pos = tell . map (WithPos ?? pos)

typingExpr ::
  ( MonadState Env m,
    MonadUniq m,
    MonadMalgo m
  ) =>
  Expr (Id ()) ->
  WriterT [WithPos] m Type
typingExpr (Var _ x) = lookupVar x
typingExpr Int {} = pure intTy
typingExpr Float {} = pure floatTy
typingExpr Bool {} = pure boolTy
typingExpr Char {} = pure charTy
typingExpr String {} = pure stringTy
typingExpr (Tuple _ xs) = tupleTy <$> mapM typingExpr xs
typingExpr (Array pos xs) = do
  ts <- mapM typingExpr xs
  ty <- newTyMeta
  addCs pos (toList $ fmap (ty :~) ts)
  pure $ arrayTy ty
typingExpr (MakeArray pos initNode sizeNode) = do
  initTy <- typingExpr initNode
  sizeTy <- typingExpr sizeNode
  addCs pos [TyApp IntC [] :~ sizeTy]
  pure $ arrayTy initTy
typingExpr (ArrayRead pos arr ix) = do
  arrTy <- typingExpr arr
  resultTy <- newTyMeta
  ixTy <- typingExpr ix
  addCs pos [TyApp ArrayC [resultTy] :~ arrTy, ixTy :~ intTy]
  pure resultTy
typingExpr (ArrayWrite pos arr ix val) = do
  arrTy <- typingExpr arr
  ixTy <- typingExpr ix
  valTy <- typingExpr val
  addCs pos [ixTy :~ TyApp IntC [], arrTy :~ TyApp ArrayC [valTy]]
  pure $ tupleTy []
typingExpr (Call pos fn args) = do
  fnTy <- typingExpr fn
  argTypes <- mapM typingExpr args
  retTy <- newTyMeta
  addCs pos [argTypes :-> retTy :~ fnTy]
  pure retTy
typingExpr (Fn _ params body) = do
  (paramIds, paramTypes) <- sTypeScope (mapAndUnzipM (rtraverse toType') params)
  zipWithM_ (\p t -> defineVar p $ Forall [] t) paramIds paramTypes
  retType <- typingExpr body
  pure $ paramTypes :-> retType
typingExpr (Seq _ e1 e2) = typingExpr e1 >> typingExpr e2
typingExpr (Let _ (ValDec pos name mtyp val) body) = do
  env <- get
  valType <- typingExpr val
  (_, cs) <- listen $
    for_ mtyp $ \typ -> do
      typ' <- sTypeScope (toType typ)
      addCs pos [valType :~ typ']
  -- value restriction
  if isValue val then letVar env name valType cs else defineVar name (Forall [] valType)
  typingExpr body
typingExpr (Let _ (ExDec _ name typ _) body) = do
  env <- get
  typ' <- sTypeScope (toType typ)
  letVar env name typ' []
  typingExpr body
typingExpr (Let _ (FunDec fs) e) = do
  env <- get
  for_ fs $ \(_, f, _, _, _) -> defineVar f . Forall [] =<< newTyMeta
  for_ fs $ \(pos, f, params, mretType, body) -> do
    (retType, (paramIds, paramTypes)) <-
      sTypeScope $
        (,)
          <$> toType' mretType
          <*> mapAndUnzipM (rtraverse toType') params
    zipWithM_ (\p t -> defineVar p $ Forall [] t) paramIds paramTypes
    (t, cs0) <- listen $ typingExpr body
    tv <- lookupVar f
    let cs1 = [tv :~ paramTypes :-> retType, t :~ retType]
    addCs pos cs1
    letVar env f tv $ map (WithPos ?? pos) cs1 <> cs0
  typingExpr e
typingExpr (If pos c t f) = do
  ct <- typingExpr c
  tt <- typingExpr t
  ft <- typingExpr f
  addCs pos [ct :~ boolTy, tt :~ ft]
  pure ft
typingExpr (BinOp pos op x y) = do
  opType <- typingOp op
  xt <- typingExpr x
  yt <- typingExpr y
  resultType <- newTyMeta
  addCs pos [opType :~ [xt, yt] :-> resultType]
  pure resultType
  where
    typingOp Add = pure $ [intTy, intTy] :-> intTy
    typingOp Sub = pure $ [intTy, intTy] :-> intTy
    typingOp Mul = pure $ [intTy, intTy] :-> intTy
    typingOp Div = pure $ [intTy, intTy] :-> intTy
    typingOp Mod = pure $ [intTy, intTy] :-> intTy
    typingOp FAdd = pure $ [floatTy, floatTy] :-> floatTy
    typingOp FSub = pure $ [floatTy, floatTy] :-> floatTy
    typingOp FMul = pure $ [floatTy, floatTy] :-> floatTy
    typingOp FDiv = pure $ [floatTy, floatTy] :-> floatTy
    typingOp Eq = newTyMeta >>= \a -> pure $ [a, a] :-> boolTy
    typingOp Neq = newTyMeta >>= \a -> pure $ [a, a] :-> boolTy
    typingOp Lt = newTyMeta >>= \a -> pure $ [a, a] :-> boolTy
    typingOp Gt = newTyMeta >>= \a -> pure $ [a, a] :-> boolTy
    typingOp Le = newTyMeta >>= \a -> pure $ [a, a] :-> boolTy
    typingOp Ge = newTyMeta >>= \a -> pure $ [a, a] :-> boolTy
    typingOp And = pure $ [boolTy, boolTy] :-> boolTy
    typingOp Or = pure $ [boolTy, boolTy] :-> boolTy
typingExpr (Match pos scrutinee clauses) = do
  ty1 <- typingExpr scrutinee
  let (pats, exprs) = unzip clauses
  mapM_ (typingPat ty1) pats
  t :| ts <- mapM typingExpr exprs
  addCs pos $ map (t :~) ts
  pure t

typingPat ::
  ( MonadState Env m,
    MonadUniq m,
    MonadMalgo m
  ) =>
  Type ->
  Pat (Id ()) ->
  WriterT [WithPos] m ()
typingPat ty (VarP _ x) = defineVar x (Forall [] ty)
typingPat ty (TupleP pos ps) = do
  vs <- replicateM (length ps) newTyMeta
  addCs pos [ty :~ TyApp TupleC vs]
  zipWithM_ typingPat vs ps

isValue :: Expr a -> Bool
isValue Var {} = True
isValue Int {} = True
isValue Float {} = True
isValue Bool {} = True
isValue Char {} = True
isValue String {} = True
isValue Fn {} = True
isValue (Tuple _ xs) = all isValue xs
isValue _ = False

type STypeVarEnv = Map (Id ()) Type

sTypeScope :: Monad m => StateT STypeVarEnv m a -> m a
sTypeScope m = evalStateT m mempty

toType' :: MonadUniq m => Maybe (SType (Id ())) -> StateT STypeVarEnv m Type
toType' mt = liftM2 fromMaybe newTyMeta $ traverse toType mt

toType :: MonadUniq m => SType (Id ()) -> StateT STypeVarEnv m Type
toType (TyVar x) = do
  kvs <- get
  (fromMaybe ?? kvs ^. at x) <$> do
    t <- newTyMeta
    modify (at x ?~ t)
    pure t
toType TyInt = pure intTy
toType TyFloat = pure floatTy
toType TyBool = pure boolTy
toType TyChar = pure charTy
toType TyString = pure stringTy
toType (TyFun ps r) = (:->) <$> mapM toType ps <*> toType r
toType (TyTuple xs) = tupleTy <$> mapM toType xs
toType (TyArray x) = arrayTy <$> toType x
