{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.TypeOf where

import           Control.Lens                      (use)
import           Control.Monad.Reader
import           Control.Monad.State.Class         (MonadState (..))
import qualified Data.Map.Strict                   as Map
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.FrontEnd.TypeCheck (TcLclEnv (..), generalize,
                                                    instantiate, unify)
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Language.Malgo.Type

typeOf :: (MonadMalgo m, MonadState RnTcEnv m) => Expr Id -> m (TypeScheme Id)
typeOf = flip runReaderT (TcLclEnv []) . typeOf'
 where
  typeOf' (Var _ x) = do
    vm <- use variableMap
    case Map.lookup x vm of
      Just ts -> return ts
      Nothing -> error "unreachable(typeOf)"
  typeOf' (Literal _ Int{}   ) = return $ Forall [] intType
  typeOf' (Literal _ Float{} ) = return $ Forall [] doubleType
  typeOf' (Literal _ Bool{}  ) = return $ Forall [] boolType
  typeOf' (Literal _ Char{}  ) = return $ Forall [] charType
  typeOf' (Literal _ String{}) = return $ Forall [] stringType
  typeOf' (BinOp _ Add  _ _  ) = return $ Forall [] intType
  typeOf' (BinOp _ Sub  _ _  ) = return $ Forall [] intType
  typeOf' (BinOp _ Mul  _ _  ) = return $ Forall [] intType
  typeOf' (BinOp _ Div  _ _  ) = return $ Forall [] intType
  typeOf' (BinOp _ Mod  _ _  ) = return $ Forall [] intType
  typeOf' (BinOp _ FAdd _ _  ) = return $ Forall [] doubleType
  typeOf' (BinOp _ FSub _ _  ) = return $ Forall [] doubleType
  typeOf' (BinOp _ FMul _ _  ) = return $ Forall [] doubleType
  typeOf' (BinOp _ FDiv _ _  ) = return $ Forall [] doubleType
  typeOf' (BinOp _ Eq   _ _  ) = return $ Forall [] boolType
  typeOf' (BinOp _ Neq  _ _  ) = return $ Forall [] boolType
  typeOf' (BinOp _ Lt   _ _  ) = return $ Forall [] boolType
  typeOf' (BinOp _ Gt   _ _  ) = return $ Forall [] boolType
  typeOf' (BinOp _ Le   _ _  ) = return $ Forall [] boolType
  typeOf' (BinOp _ Ge   _ _  ) = return $ Forall [] boolType
  typeOf' (BinOp _ And  _ _  ) = return $ Forall [] boolType
  typeOf' (BinOp _ Or   _ _  ) = return $ Forall [] boolType
  typeOf' (If    _ _    e _  ) = typeOf' e
  typeOf' (Let   _ _  e      ) = typeOf' e
  typeOf' (Apply _ e1 e2     ) = do
    retType <- TyMeta <$> newTyRef
    e1Type  <- instantiate =<< typeOf' e1
    e2Type  <- instantiate =<< typeOf' e2
    unify noSrcSpan e1Type (e2Type --> retType)
    generalize retType
  typeOf' (Tuple _ xs) =
    generalize =<< tupleType <$> (mapM instantiate =<< mapM typeOf' xs)
  typeOf' Fn{} = error "unreachable(typeOf')"
