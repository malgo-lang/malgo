{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.FrontEnd.Typing.Infer
  ( Typing
  , generalize
  )
where

import           Language.Malgo.Id
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude                    hiding (ix, op)

import           Language.Malgo.IR.Syntax

import           Language.Malgo.TypeRep.SType
import           Language.Malgo.TypeRep.Type

import           Language.Malgo.FrontEnd.Typing.Constraint
import           Language.Malgo.FrontEnd.Typing.Subst

import           Data.Set                                  ((\\))
import           Text.Parsec.Pos                           (SourcePos)

data Typing

instance Pass Typing (Expr (Id ())) (Expr (Id Type)) where
  passName = "Typing"
  isDump   = dumpTyped
  trans e = evalStateT ?? mempty $ do
    _   <- typingExpr e
    env <- get

    opt <- getOpt
    when (dumpTypeTable opt) $ dump (toList env)

    pure $ fmap (\x -> maybe (x & metaL .~ Kind) (fmap removeExplictForall) (env ^. at x)) e

type Env = IdMap () (Id Scheme)

newTyMeta :: MonadUniq m => m Type
newTyMeta = TyMeta <$> getUniq

generalize :: Env -> Type -> Scheme
generalize env t = Forall (toList $ ftv t \\ ftv env) t

letVar :: MonadState Env m => Env -> Id () -> Type -> m ()
letVar env var ty = defineVar var $ generalize env ty

defineVar :: MonadState Env m => Id () -> Scheme -> m ()
defineVar x t = modify (at x ?~ x { idMeta = t })

lookupVar :: (MonadState Env f, MonadUniq f) => Id () -> f Type
lookupVar x = do
  env <- get
  case env ^. at x of
    Just Id { idMeta = scheme } -> instantiate scheme
    Nothing                     -> bug Unreachable

updateSubst :: (MonadState Env m, MonadMalgo m)
            => SourcePos
            -> [Constraint]
            -> WriterT Subst m ()
updateSubst pos cs = case solve cs of
  Right subst -> tell subst >> modify (apply subst)
  Left  e     -> malgoError pos "typing" e

runSubst :: (MonadState Env m, Substitutable a) => WriterT Subst m a -> m a
runSubst m = do
  (x, subst) <- runWriterT m
  modify (apply subst)
  pure (apply subst x)

typingExpr :: (MonadState Env m, MonadUniq m, MonadMalgo m) => Expr (Id ()) -> m Type
typingExpr (Var _ x)      = lookupVar x
typingExpr Int{}          = pure intTy
typingExpr Float{}        = pure floatTy
typingExpr Bool{}         = pure boolTy
typingExpr Char{}         = pure charTy
typingExpr String{}       = pure stringTy
typingExpr (Tuple _   xs) = tupleTy <$> mapM typingExpr xs
typingExpr (Array pos xs) = runSubst $ do
  ts <- mapM typingExpr xs
  ty <- newTyMeta
  updateSubst pos (toList $ fmap (ty :~) ts)
  pure $ arrayTy ty
typingExpr (MakeArray pos initNode sizeNode) = runSubst $ do
  initTy <- typingExpr initNode
  sizeTy <- typingExpr sizeNode
  updateSubst pos [TyApp IntC [] :~ sizeTy]
  pure $ arrayTy initTy
typingExpr (ArrayRead pos arr ix) = runSubst $ do
  arrTy    <- typingExpr arr
  resultTy <- newTyMeta
  ixTy     <- typingExpr ix
  updateSubst pos [TyApp ArrayC [resultTy] :~ arrTy, ixTy :~ intTy]
  pure resultTy
typingExpr (ArrayWrite pos arr ix val) = runSubst $ do
  arrTy <- typingExpr arr
  ixTy  <- typingExpr ix
  valTy <- typingExpr val
  updateSubst pos [ixTy :~ TyApp IntC [], arrTy :~ TyApp ArrayC [valTy]]
  pure $ tupleTy []
typingExpr (Call pos fn args) = runSubst $ do
  fnTy     <- typingExpr fn
  argTypes <- mapM typingExpr args
  retTy    <- newTyMeta
  updateSubst pos [TyApp FunC (retTy : argTypes) :~ fnTy]
  pure retTy
typingExpr (Fn _ params body) = do
  paramTypes <- evalStateT ?? mempty $ traverse
    (\(_, t) -> fromMaybe <$> newTyMeta <*> traverse toType t)
    params
  zipWithM_ (\(p, _) t -> defineVar p $ Forall [] t) params paramTypes
  retType <- typingExpr body
  pure $ paramTypes --> retType
typingExpr (Seq _ e1                         e2  ) = typingExpr e1 >> typingExpr e2
typingExpr (Let _ (ValDec pos name mtyp val) body) = runSubst $ do
  env     <- get
  valType <- typingExpr val

  for_ mtyp $ \typ -> do
    typ' <- evalStateT (toType typ) mempty
    updateSubst pos [valType :~ typ']

  -- value restriction
  if isValue val then letVar env name valType else defineVar name (Forall [] valType)

  typingExpr body
typingExpr (Let _ (ExDec _ name typ _) body) = do
  env  <- get
  typ' <- evalStateT (toType typ) mempty
  letVar env name typ'
  -- defineVar name (Forall [] typ')
  typingExpr body
typingExpr (Let _ (FunDec fs) e) = do
  env <- get
  for_ fs $ \(_, f, _, _, _) -> defineVar f . Forall [] =<< newTyMeta
  for_ fs $ \(pos, f, params, mretType, body) -> letVar env f <=< runSubst $ do
    (paramTypes, s) <- runStateT ?? mempty $ traverse
      (\(_, t) -> fromMaybe <$> newTyMeta <*> traverse toType t)
      params
    retType <- fromMaybe <$> newTyMeta <*> evalStateT (mapM toType mretType) s

    zipWithM_ (\(p, _) t -> defineVar p $ Forall [] t) params paramTypes

    t  <- typingExpr body
    tv <- lookupVar f
    updateSubst pos [tv :~ TyApp FunC (retType : paramTypes), t :~ retType]
    pure tv

  typingExpr e
typingExpr (If pos c t f) = runSubst $ do
  ct <- typingExpr c
  tt <- typingExpr t
  ft <- typingExpr f
  updateSubst pos [ct :~ boolTy, tt :~ ft]
  pure ft
typingExpr (BinOp pos op x y) = runSubst $ do
  opType     <- typingOp op
  xt         <- typingExpr x
  yt         <- typingExpr y
  resultType <- newTyMeta
  updateSubst pos [opType :~ [xt, yt] --> resultType]
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
typingExpr (Match pos scrutinee clauses) = runSubst $ do
  ty1 <- typingExpr scrutinee
  let (pats, exprs) = unzip clauses
  mapM_ (typingPat pos ty1) pats
  t :| _ <- mapM typingExpr exprs
  pure t

typingPat :: (MonadMalgo m, MonadState Env m, MonadUniq m)
          => SourcePos
          -> Type
          -> Pat (Id ())
          -> WriterT Subst m ()
typingPat _   ty (VarP   x ) = defineVar x (Forall [] ty)
typingPat pos ty (TupleP ps) = do
  vs <- replicateM (length ps) newTyMeta
  updateSubst pos [ty :~ TyApp TupleC vs]
  zipWithM_ (typingPat pos) vs ps

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

toType :: MonadUniq m => SType (Id ()) -> StateT (Map (Id ()) Type) m Type
toType (TyVar x) = do
  kvs <- get
  (fromMaybe ?? kvs ^. at x) <$> do
    t <- newTyMeta
    modify (at x ?~ t)
    pure t
toType TyInt        = pure intTy
toType TyFloat      = pure floatTy
toType TyBool       = pure boolTy
toType TyChar       = pure charTy
toType TyString     = pure stringTy
toType (TyFun ps r) = (-->) <$> mapM toType ps <*> toType r
toType (TyTuple xs) = tupleTy <$> mapM toType xs
toType (TyArray x ) = arrayTy <$> toType x
