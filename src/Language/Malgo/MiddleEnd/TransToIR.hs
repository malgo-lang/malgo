{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Malgo.MiddleEnd.TransToIR (trans) where

import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import qualified Language.Malgo.IR.Syntax as S
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Type
import           Language.Malgo.TypedID

data TEnv = TEnv { _externVars :: [Defn (ID MType)]
                 , _uniqSupply :: UniqSupply
                 }

makeLenses ''TEnv

instance MalgoEnv TEnv where
  uniqSupplyL = uniqSupply
  genEnv = TEnv []

throw :: MonadMalgo TEnv m => Doc ann -> m a
throw mes = malgoError $ "error(transToIR):" <+> mes

update :: MonadMalgo TEnv m => ID Type -> m (ID MType)
update a = do
  mty <- toMType (typeOf a)
  return $ a & idMeta .~ mty

newTmp :: MonadMalgo TEnv m => Name -> MType -> m (ID MType)
newTmp n t = ID ("$" <> n) <$> newUniq <*> return t

trans :: MonadMalgo TEnv m => S.Expr TypedID -> m (Expr (ID MType), [Defn (ID MType)])
trans e = do
  e' <- transToIR e
  env <- getEnv
  return (e', env ^. externVars)

insertLet :: MonadMalgo TEnv m => S.Expr (ID Type) -> (ID MType -> m (Expr (ID MType))) -> m (Expr (ID MType))
insertLet (S.Var _ x) k = update x >>= k
insertLet v k = do
  ty <- toMType $ typeOf v
  x <- newTmp "k" ty
  v' <- transToIR v
  e <- k x
  return (Let x v' e)

transToIR :: MonadMalgo TEnv m => S.Expr TypedID -> m (Expr (ID MType))
transToIR (S.Var _ a)   = Var <$> update a
transToIR (S.Int _ x)   = return (Int x)
transToIR (S.Float _ x) = return (Float x)
transToIR (S.Bool _ x)  = return (Bool x)
transToIR (S.Char _ c)  = return (Char c)
transToIR (S.String _ s) = return (String s)
transToIR (S.Unit _) = return Unit
transToIR (S.Tuple _ vs) = bind vs [] (return . Tuple)
  where
    bind [] args k = k (reverse args)
    bind (x:xs) args k =
      insertLet x (\x' -> bind xs (x' : args) k)
transToIR (S.TupleAccess _ e i) =
  insertLet e (\e' -> return $ Access e' [0, i])
transToIR (S.Fn _ params body) = do
  body' <- transToIR body
  fnty <- toMType (FunTy (map snd params) (typeOf body))
  fnid <- newTmp "lambda" fnty
  return (Let fnid body' (Var fnid))

toMType :: MonadMalgo TEnv m => Type -> m MType
toMType (NameTy n) =
  case n of
    "Int"    -> return $ IntTy 32
    "Float"  -> return DoubleTy
    "Bool"   -> return $ IntTy 1
    "Char"   -> return $ IntTy 8
    "String" -> return $ PointerTy (IntTy 8)
    "Unit"   -> return $ StructTy []
    _        -> throw $ pretty n <+> "is not valid type"
toMType (FunTy params ret) =
  FunctionTy <$> toMType ret <*> mapM toMType params
toMType (TupleTy xs) =
  PointerTy . StructTy <$> mapM toMType xs
toMType ClsTy{} =
  throw "ClsTy does not have MType"
