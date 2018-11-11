{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Language.Malgo.MiddleEnd.TypeOf (typeOf) where

import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.FrontEnd.TypeCheck (TcLclEnv (..), generalize,
                                                    instantiate, unify)
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Language.Malgo.Type
import           Universum                         hiding (Type)

typeOf :: (MonadMalgo m, MonadState RnTcEnv m) => Expr Id -> m (TypeScheme Id)
typeOf = usingReaderT (TcLclEnv []) undefined
