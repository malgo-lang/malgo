{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
module Language.Malgo.MiddleEnd.TransToLIR ( TransToLIR ) where

import           Language.Malgo.ID
import           Language.Malgo.IR.LIR       as L
import           Language.Malgo.IR.MIR       as M
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type as M
import           Relude                      hiding (Type)
import           Relude.Extra.Map            hiding (size)

data TransToLIR

instance Pass TransToLIR (M.Program M.Type (ID M.Type)) (L.Program (ID L.Type)) where
  isDump _ = False -- TODO: support dump LIR
  trans M.Program { functions, mainExpr } = undefined

data Env = Env { variableMap :: Map (ID M.Type) (ID L.Type)
               , functionMap :: Map (ID M.Type) (ID L.Type)
               , terminator  :: ID L.Type -> GenExpr ()
               , captures    :: ID L.Type
               }

type GenExpr a = StateT [(ID L.Type, L.Inst (ID L.Type))] (ReaderT Env MalgoM) a
type GenProgram a = ReaderT Env MalgoM a

convertType :: M.Type -> L.Type
-- convertType (TyApp FunC (r:ps)) =
--   L.ptr $ L.StructureType False [L.ptr $ L.FunctionType (convertType r) (L.ptr L.i8 : map convertType ps) False, L.ptr L.i8]
-- convertType (TyApp IntC []) = L.i64
-- convertType (TyApp FloatC []) = L.double
-- convertType (TyApp BoolC []) = L.i1
-- convertType (TyApp CharC []) = L.i8
-- convertType (TyApp StringC []) = L.ptr L.i8
-- convertType (TyApp TupleC xs) = L.ptr (L.StructureType False (map convertType xs))
-- convertType (TyApp ArrayC [x]) = L.ptr (convertType x)
convertType t = error $ fromString $ "unreachable(convertType): " <> dumpStr t

getVar :: MonadReader Env m => ID M.Type -> m (ID L.Type)
getVar i = do
  m <- asks variableMap
  case lookup i m of
    Just x  -> pure x
    Nothing -> error $ show i <> " is not found in " <> show m

getFun i = do
  m <- asks functionMap
  case lookup i m of
    Just x  -> pure x
    Nothing -> error $ show i <> " is not found in " <> show m

push :: MonadState [(ID L.Type, L.Inst (ID L.Type))] m => ID L.Type -> L.Inst (ID L.Type) -> m ()
push var inst = modify (<> [(var, inst)])

constInt64 x = do
  x' <- newID L.I64 "x"
  push x' $ L.Const (Int64 x)
  pure x'

alloca ty size = do
  i <- newID (L.Ptr $ convertType ty) "alloca"
  push i $ Alloca (convertType ty) size
  pure i
