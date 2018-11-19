{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
module Language.Malgo.MiddleEnd.Closure
  ( trans
  )
where

import           Control.Lens                    (makeLenses)
import           Data.Outputable
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.Id
import qualified Language.Malgo.IR.AST           as AST
import           Language.Malgo.IR.MIR
import           Language.Malgo.Monad
import           Language.Malgo.Type
import           Universum                       hiding (Type)

data ClsEnv = ClsEnv { _typeEnv :: Map Id TypeRep
                     , _defs    :: [Def]
                     } deriving (Show, Generic, Outputable)
makeLenses ''ClsEnv

data ClsInfo = ClsInfo { _rnTcEnv :: RnTcEnv
                       , _knowns  :: [Id]
                       } deriving (Show, Generic, Outputable)
makeLenses ''ClsInfo

type Trans m = (MonadReader ClsInfo m, MonadState ClsEnv m, MonadMalgo m)

trans :: MonadMalgo m => RnTcEnv -> [(Id, [Id], AST.Expr Id)] -> m [Def]
trans rte xs =
  map (view defs)
    $ usingReaderT (ClsInfo rte [])
    $ executingStateT (ClsEnv mempty [])
    $ mapM_ transDef xs

transDef :: Trans m => (Id, [Id], AST.Expr Id) -> m ()
transDef = undefined

transType :: Trans m => TypeScheme Id -> m TypeRep
transType = undefined
