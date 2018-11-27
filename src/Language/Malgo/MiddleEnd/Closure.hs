{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeOperators             #-}
module Language.Malgo.MiddleEnd.Closure
  ( trans
  )
where

import           Control.Lens                    (makeLenses)
import           Control.Monad.Writer.Strict     (MonadWriter (..), execWriterT)
import qualified Data.Map.Strict                 as Map
import           Data.Outputable
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.Id
import qualified Language.Malgo.IR.AST           as AST
import           Language.Malgo.IR.MIR
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.Type
import           Language.Malgo.Utils
import           Universum                       hiding (Type)

data ClsEnv = ClsEnv { _typeEnv :: Map Id TypeRep
                     , _defs    :: [Def]
                     } deriving (Show, Generic, Outputable)
makeLenses ''ClsEnv

data ClsInfo = ClsInfo { _rnTcEnv :: RnTcEnv
                       , _knowns  :: [Id]
                       , _clsMap  :: Map Id Id -- Map <元の関数名> <対応するクロージャ変数名>
                       } deriving (Show, Generic, Outputable)
makeLenses ''ClsInfo

pushDef f xs b = modify $ over defs (Def f xs b :)

addTypeEnv x t = modify $ over typeEnv $ Map.insert x t

trans :: MonadMalgo m => RnTcEnv -> [(Id, [Id], AST.Expr Id)] -> m [Def]
trans rte xs =
  map (view defs)
    $ usingReaderT (ClsInfo rte [] mempty)
    $ executingStateT (ClsEnv mempty [])
    $ mapM_ transDef xs

transDef (f, xs, e) = do
  e' <- local (over knowns ((f:xs) <>)) $ transBlock e
  pushDef f xs e'

lookup :: Id -> Map Id a -> a
lookup x m =
  fromMaybe (error $ show $ "unreachable(lookup):" <+> pPrint x) (Map.lookup x m)

transBlock e = appEndo <$> execWriterT (transExpr e) <*> pure []

{-
Language.Malgo.MiddleEnd.FlattenでflatなASTに変形されている。
let x = v in eについて、
transFlatExprがvをExprに変換し、
transExprが全体をLet x = v' : transExpr eに変換する
-}
transExpr (AST.Let _ (AST.NonRec _ x _ v) e) = do
  t <- transType =<< lookup x <$> view (rnTcEnv . variableMap)
  addTypeEnv x t
  v' <- transFlatExpr v
  tell $ Endo (Let x v' :)
  transExpr e
transExpr (AST.Let _ _ _) = undefined
transExpr x = do
  x' <- transFlatExpr x
  tell $ Endo (Do x' :)

{-
Apply a bについて
aのTypeRepがTupleType [ArrowType BoxType _, BoxType]の場合、
( [ Let function = Access a 0
  , Let capture = Access a 1
  , Let a' = Apply function capture
  ]
, Apply a' b
)
に変換する。
-}
transFlatExpr = undefined

{- TyVarはBoxに、その他はよしなに変換する -}
transType = undefined

-- freevars :: AST.Expr ~> []
-- freevars = undefined
