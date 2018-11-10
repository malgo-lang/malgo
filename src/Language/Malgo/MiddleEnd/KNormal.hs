{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Language.Malgo.MiddleEnd.KNormal where

import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Type
import           Universum                       hiding (Type)

knormal :: MonadState RnTcEnv m => [Decl Id] -> m [Decl Id]
knormal = undefined

checkKNormalized :: [Decl Id] -> Bool
checkKNormalized = undefined
