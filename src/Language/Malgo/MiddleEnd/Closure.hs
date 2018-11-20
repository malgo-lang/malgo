{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
module Language.Malgo.MiddleEnd.Closure
  ( trans
  )
where

import           Control.Lens                    (makeLenses)
import qualified Data.Map.Strict                 as Map
import           Data.Outputable
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.Id
import qualified Language.Malgo.IR.AST           as AST
import           Language.Malgo.IR.MIR
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.Type
import           Universum                       hiding (Type)

data ClsEnv = ClsEnv { _typeEnv :: Map Id TypeRep
                     , _defs    :: [Def]
                     } deriving (Show, Generic, Outputable)
makeLenses ''ClsEnv

data ClsInfo = ClsInfo { _rnTcEnv :: RnTcEnv
                       , _knowns  :: [Id]
                       , _clsMap :: Map Id Id -- Map <元の関数名> <対応するクロージャ変数名>
                       } deriving (Show, Generic, Outputable)
makeLenses ''ClsInfo

type Trans m = (MonadReader ClsInfo m, MonadState ClsEnv m, MonadMalgo m)

pushDef :: Trans m => Id -> [Id] -> Block -> m ()
pushDef f xs b = modify $ over defs (Def f xs b :)

addTypeEnv :: Trans m => Id -> TypeRep -> m ()
addTypeEnv x t = modify $ over typeEnv $ Map.insert x t

trans :: MonadMalgo m => RnTcEnv -> [(Id, [Id], AST.Expr Id)] -> m [Def]
trans rte xs =
  map (view defs)
    $ usingReaderT (ClsInfo rte [] mempty)
    $ executingStateT (ClsEnv mempty [])
    $ mapM_ transDef xs

transDef :: Trans m => (Id, [Id], AST.Expr Id) -> m ()
transDef (f, xs, e) = do
  e' <- local (over knowns ((f:xs) <>)) $ transExpr e
  pushDef f xs e'

lookup :: Id -> Map Id a -> a
lookup x m =
  fromMaybe (error $ show $ "unreachable(lookup):" <+> pPrint x) (Map.lookup x m)

{-
Language.Malgo.MiddleEnd.FlattenでflatなASTに変形されている。
let x = v in eについて、
transFlatExprがvをExprに変換し、
transExprが全体をLet x = v' : transExpr eに変換する
-}

transExpr :: Trans m => AST.Expr Id -> m Block
transExpr (AST.Let _ (AST.NonRec _ x _ v) e) = do
  t <- transType =<< lookup x <$> view (rnTcEnv . variableMap)
  addTypeEnv x t
  (xs, v') <- transFlatExpr v
  ((xs <> [Let x v']) <>) <$> transExpr e

{-
返り値の_1はクロージャ呼び出しなど、一つのAST.Exprが複数行のInstになるときに用いる。

例えば、Apply a bについて
aのTypeRepがTupleType [ArrowType BoxType _, BoxType]の場合、
( [ Let function = Access a 0
  , Let capture = Access a 1
  , Let a' = Apply function capture
  ]
, Apply a' b
)
に変換する。
-}
transFlatExpr :: Trans m => AST.Expr Id -> m ([Inst], Expr)
transFlatExpr = undefined

{- TyVarはBoxに、その他はよしなに変換する -}
transType :: Trans m => TypeScheme Id -> m TypeRep
transType = undefined

freevars :: AST.Expr Id -> [Id]
freevars = undefined
