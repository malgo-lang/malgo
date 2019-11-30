{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.FrontEnd.Typing.Infer () where

import           Language.Malgo.FrontEnd.Info
import           Language.Malgo.FrontEnd.Typing.Constraint
import           Language.Malgo.FrontEnd.Typing.Subst
import           Language.Malgo.ID
import           Language.Malgo.IR.Syntax                  hiding (info)
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import           Relude                                    hiding (Constraint,
                                                            Type)

data Typing

instance Pass Typing (Expr RawID) (Expr TypedID) where
  isDump = dumpTyped
  trans s = usingReaderT mempty $ do
    expr <- typingExpr s
    undefined

type Env = Map Text Type

type InferM a = ReaderT Env MalgoM a

throw :: Info -> Doc -> InferM a
throw info mes = malgoError $ "error(typing):" <+> pPrint info <+> mes

typingExpr :: Expr RawID -> InferM (Expr TypedID)
typingExpr = undefined
