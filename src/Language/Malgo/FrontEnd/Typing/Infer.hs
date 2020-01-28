{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.FrontEnd.Typing.Infer
  ( Typing
  )
where

import           Language.Malgo.FrontEnd.Info
import           Language.Malgo.FrontEnd.Typing.Constraint
import           Language.Malgo.FrontEnd.Typing.Subst
import           Language.Malgo.ID
import           Language.Malgo.IR.Syntax
                                         hiding ( info )
import qualified Language.Malgo.IR.Syntax      as Syntax
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty   hiding ( first )
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.Prelude
import           Relude.Unsafe                  ( fromJust )

data Typing

instance Pass Typing (Expr (ID ())) (Expr (ID Type)) where
  passName = "Typing"
  isDump   = dumpTyped
  trans e = evaluatingStateT mempty $ do
    (_, cs) <- runWriterT $ typingExpr e
    subst   <- catchUnifyError (Syntax.info e) "toplevel" =<< solve cs
    env     <- gets (apply subst)

    opt     <- liftMalgo $ asks maOption
    when (dumpTypeTable opt) $ do
      let xs = map (\x@ID { idMeta } -> (x, idMeta)) (toList env)
      liftMalgo $ dump xs

    pure $ fmap (\x -> removeExplictForall <$> fromJust (lookup x env)) e

type Env = IDMap () (ID Type)

throw :: Info -> Doc -> b
throw info mes = errorDoc $ "error(typing):" <+> pPrint info $+$ mes

catchUnifyError :: Applicative f => Info -> Doc -> Either UnifyError a -> f a
catchUnifyError i name (Left (MismatchConstructor c1 c2)) =
  throw i $ "mismatch constructor" <+> pPrint c1 <> "," <+> pPrint c2 $+$ "on" <+> name
catchUnifyError i name (Left (MismatchLength ts1 ts2)) =
  throw i $ "mismatch length" <+> pPrint ts1 <> "," <+> pPrint ts2 $+$ "on" <+> name
catchUnifyError i name (Left (InfinitType var ty)) =
  throw i $ "infinit type" <+> pPrint var <> "," <+> pPrint ty $+$ "on" <+> name
catchUnifyError i name (Left (MismatchLevel ty1 ty2)) =
  throw i $ "mismatch level" <+> pPrint ty1 <> "," <+> pPrint ty2 $+$ "on" <+> name
catchUnifyError _ _ (Right a) = pure a

newTyMeta :: MonadMalgo m => m Type
newTyMeta = TyMeta <$> newUniq

generalize :: Env -> Type -> Type
generalize env t | null fv   = t
                 | otherwise = TyForall fv t
  where fv = toList $ ftv t \\ ftv env

letVar :: (MonadState Env m, MonadMalgo m) => Info -> Env -> ID () -> Type -> [Constraint] -> m ()
letVar info env var ty cs = do
  subst <- catchUnifyError info (pPrint var) =<< solve cs
  let sc = generalize (apply subst env) (apply subst ty)
  defineVar var sc
  modify (apply subst)

defineVar :: MonadState Env m => ID () -> Type -> m ()
defineVar x t = modify (insert x $ x { idMeta = t })

lookupVar :: MonadState Env f => ID () -> f Type
lookupVar x = typeOf . fromJust . lookup x <$> get

typingExpr :: (MonadWriter [Constraint] f, MonadState Env f, MonadMalgo f) => Expr (ID ()) -> f Type
typingExpr (Var _ x)    = lookupVar x
typingExpr Int{}        = pure $ TyApp IntC []
typingExpr Float{}      = pure $ TyApp FloatC []
typingExpr Bool{}       = pure $ TyApp BoolC []
typingExpr Char{}       = pure $ TyApp CharC []
typingExpr String{}     = pure $ TyApp StringC []
typingExpr Unit{}       = pure $ TyApp TupleC []
typingExpr (Tuple _ xs) = do
  ts <- mapM typingExpr xs
  pure $ TyApp TupleC ts
typingExpr (TupleAccess _ e _) = do
  -- FIXME: 正しく型付けする
  _ <- typingExpr e
  newTyMeta
typingExpr (MakeArray _ initNode sizeNode) = do
  initTy <- instantiate =<< typingExpr initNode
  sizeTy <- instantiate =<< typingExpr sizeNode
  tell [TyApp IntC [] :~ sizeTy]
  pure $ TyApp ArrayC [initTy]
typingExpr (ArrayRead _ arr _) = do
  arrTy    <- instantiate =<< typingExpr arr
  resultTy <- newTyMeta
  tell [TyApp ArrayC [resultTy] :~ arrTy]
  pure resultTy
typingExpr (ArrayWrite _ arr ix val) = do
  arrTy <- instantiate =<< typingExpr arr
  ixTy  <- instantiate =<< typingExpr ix
  valTy <- instantiate =<< typingExpr val
  tell [ixTy :~ TyApp IntC [], arrTy :~ TyApp ArrayC [valTy]]
  pure $ TyApp TupleC []
typingExpr (Call _ fn args) = do
  fnTy     <- inst1 =<< typingExpr fn
  argTypes <- traverse instantiate =<< mapM typingExpr args
  retTy    <- newTyMeta
  tell [TyApp FunC (retTy : argTypes) :~ fnTy]
  pure retTy
typingExpr (Fn i params body) = do
  env        <- get
  paramTypes <- mapM (\(_, paramType) -> paramType `whenNothing` newTyMeta) params
  mapM_ (\((p, _), t) -> defineVar p t) (zip params paramTypes)
  (t, cs) <- listen $ typingExpr body
  sub     <- catchUnifyError i "function literal" =<< solve cs
  pure $ generalize (apply sub env) $ apply sub (TyApp FunC (t : paramTypes))
typingExpr (Seq i e1 e2) = do
  (_, cs1) <- listen $ typingExpr e1
  _        <- catchUnifyError i "seq" =<< solve cs1
  typingExpr e2
typingExpr (Let _ (ValDec i name mtyp val) body) = do
  env            <- get

  (valType, cs1) <- listen $ typingExpr val

  cs2            <- case mtyp of
    Just typ -> tell [valType :~ typ] >> pure (valType :~ typ : cs1)
    Nothing  -> pure cs1

  -- value restriction
  if isValue val then letVar i env name valType cs2 else defineVar name valType

  typingExpr body
typingExpr (Let _ (ExDec _ name typ _) body) = do
  defineVar name typ
  typingExpr body
typingExpr (Let _ (FunDec fs) e) = do
  env <- get
  mapM_ prepare            fs
  mapM_ (typingFunDec env) fs
  typingExpr e
 where
  prepare (_, f, _, _, _) = defineVar f =<< newTyMeta
  typingFunDec env (i', f, params, mretty, body) = do
    paramTypes <- mapM (\(_, paramType) -> paramType `whenNothing` newTyMeta) params
    retType    <- mretty `whenNothing` newTyMeta

    mapM_ (\((p, _), t) -> defineVar p t) (zip params paramTypes)
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

typingPat :: (MonadState Env m, MonadMalgo m, MonadWriter [Constraint] m)
          => Type
          -> Pat (ID ())
          -> m ()
typingPat ty (VarP   x ) = defineVar x ty
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
isValue Unit{}       = True
isValue Fn{}         = True
isValue (Tuple _ xs) = all isValue xs
isValue _            = False
