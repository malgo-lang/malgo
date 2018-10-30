{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.FrontEnd.TypeCheck where

import qualified Data.Map.Strict                 as Map
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.Type
import           Universum                       hiding (Type)

typeCheckError :: SrcSpan -> Doc -> a
typeCheckError ss doc = error $ show $ "error(type check)[" <> pPrint ss <> "]" <+> doc

typeCheck :: MonadMalgo m => [Decl Id] -> m RnTcEnv
typeCheck ds = do
  env <- makeRnTcEnv
  executingStateT env $ do
    mapM_ generateHeader ds
    mapM_ loadTypeDef typeDefs
    undefined
  where
    typeDefs = mapMaybe (\case
                            TypeDef ss x ps t -> Just (ss, x, ps, t)
                            _ -> Nothing) ds
    scAnns = mapMaybe (\case
                          ScAnn ss x t -> Just (ss, x, t)
                          _ -> Nothing) ds
    scDefs = mapMaybe (\case
                          ScDef ss x ps e -> Just (ss, x, ps, e)
                          _ -> Nothing) ds

generateHeader :: (MonadState RnTcEnv m, MonadMalgo m) => Decl Id -> m ()
generateHeader d = undefined

loadTypeDef :: (MonadState RnTcEnv m, MonadMalgo m) => (SrcSpan, Id, [Id], Type Id) -> m ()
loadTypeDef (ss, x, ps, ty) = undefined

loadScAnn :: (MonadState RnTcEnv m, MonadMalgo m) => (SrcSpan, Id, Type Id) -> m ()
loadScAnn (ss, x, ty) = undefined

typeCheckScDef :: (MonadState RnTcEnv m, MonadMalgo m) => (SrcSpan, Id, [Id], Expr Id) -> m ()
typeCheckScDef (ss, x, ps, e) = undefined

typeCheckExpr :: (MonadState RnTcEnv m, MonadMalgo m) => Expr Id -> m (Type Id)
typeCheckExpr = undefined
