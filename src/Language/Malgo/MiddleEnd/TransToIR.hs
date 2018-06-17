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

throw :: MonadMalgo TEnv m => Info -> Doc ann -> m a
throw info mes = malgoError $ "error(transToIR):" <+> pretty info <+> mes

update :: ID Type -> MType -> ID MType
update a mty = a & idMeta .~ mty

newTmp :: MonadMalgo TEnv m => Name -> MType -> m (ID MType)
newTmp n t = ID ("$" <> n) <$> newUniq <*> return t

newUnused :: MonadMalgo TEnv m => m (ID MType)
newUnused = newTmp "_" (StructTy [])

trans :: MonadMalgo TEnv m => S.Expr TypedID -> m (Expr (ID MType), [Defn (ID MType)])
trans e = do
  e' <- transToIR e
  env <- getEnv
  return (e', env ^. externVars)

transToIR :: MonadMalgo TEnv m => S.Expr TypedID -> m (Expr (ID MType))
transToIR (S.Var i a)   = Var . update a <$> toMType i (typeOf a)
transToIR (S.Int _ x)   = return (Int x)
transToIR (S.Float _ x) = return (Float x)
transToIR (S.Bool _ x)  = return (Bool x)
transToIR (S.Char _ c)  = return (Char c)

toMType :: MonadMalgo TEnv m => Info -> Type -> m MType
toMType info (NameTy n) =
  case n of
    "Int"    -> return $ IntTy 32
    "Float"  -> return DoubleTy
    "Bool"   -> return $ IntTy 1
    "Char"   -> return $ IntTy 8
    "String" -> return $ PointerTy (IntTy 8)
    "Unit"   -> return $ StructTy []
    _        -> throw info $ pretty n <+> "is not valid type"
toMType info (FunTy params ret) =
  FunctionTy <$> toMType info ret <*> mapM (toMType info) params
toMType info (TupleTy xs) =
  PointerTy . StructTy <$> mapM (toMType info) xs
toMType info ClsTy{} =
  throw info "ClsTy does not have MType"
