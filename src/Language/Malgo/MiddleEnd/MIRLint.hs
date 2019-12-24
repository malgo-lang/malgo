{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.MiddleEnd.MIRLint
  ( MIRLint
  )
where

import           Language.Malgo.ID
import           Language.Malgo.IR.MIR
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.Prelude

data MIRLint

instance Pass MIRLint (Program Type (ID Type)) (Program Type (ID Type)) where
  isDump _ = False
  trans e@(Program fs _) = usingReaderT (Env fs []) (lintProgram e) >> pure e

data Env = Env { functions :: [Func Type (ID Type)]
               , variables :: [ID Type]
               }

definedVar :: (MonadReader Env m, MonadIO m) => ID Type -> m ()
definedVar a =
  unlessM (elem a <$> asks variables) (errorDoc $ pPrint a <+> "is not defined")

notDefinedVar :: (MonadReader Env m, MonadIO m) => ID Type -> m ()
notDefinedVar a = unlessM (notElem a <$> asks variables)
                          (errorDoc $ pPrint a <+> "is already defined")

isKnownFunc :: (MonadReader Env m, MonadIO m) => ID Type -> m ()
isKnownFunc a = do
  Env { functions } <- ask
  let mfunc = find (\Func { name } -> name == a) functions
  case mfunc of
    Just Func { captures = Nothing } -> pass
    _ -> errorDoc $ pPrint a <+> "is not known function"

isUnknownFunc :: (MonadReader Env m, MonadIO m) => ID Type -> m ()
isUnknownFunc a = do
  Env { functions } <- ask
  let mfunc = find (\Func { name } -> name == a) functions
  case mfunc of
    Just Func { captures = Just _ } -> pass
    _ -> errorDoc $ pPrint a <+> "is not known function"

lintProgram :: (MonadReader Env m, MonadIO m) => Program Type (ID Type) -> m ()
lintProgram Program { functions, mainExpr } = do
  mapM_ lintFunc functions
  lintExpr mainExpr

lintFunc :: (MonadReader Env m, MonadIO m) => Func Type (ID Type) -> m ()
lintFunc Func { name = _, captures, mutrecs, params, body } =
  local (\e -> e { variables = fromMaybe [] captures <> params <> mutrecs })
    $ lintExpr body

lintExpr :: (MonadReader Env m, MonadIO m) => Expr Type (ID Type) -> m ()
lintExpr (Var   a           ) = definedVar a
lintExpr (Lit   _           ) = pass
lintExpr (Tuple xs          ) = mapM_ definedVar xs
lintExpr (TupleAccess x   _ ) = definedVar x
lintExpr (MakeArray   _   x ) = definedVar x
lintExpr (ArrayRead   arr ix) = definedVar arr >> definedVar ix
lintExpr (ArrayWrite arr ix val) =
  definedVar arr >> definedVar ix >> definedVar val
lintExpr (MakeClosure      f  xs) = isUnknownFunc f >> mapM_ definedVar xs
lintExpr (CallDirect       f  xs) = isKnownFunc f >> mapM_ definedVar xs
lintExpr (CallWithCaptures f  xs) = isUnknownFunc f >> mapM_ definedVar xs
lintExpr (CallClosure      f  xs) = definedVar f >> mapM_ definedVar xs
lintExpr (Let              xs e ) = do
  mapM_ notDefinedVar ns
  local (\env -> env { variables = ns <> variables env })
        (mapM_ lintExpr vs >> lintExpr e)
  where (ns, vs) = unzip xs
lintExpr (If    _ t f ) = lintExpr t >> lintExpr f
lintExpr (Prim  _ _ xs) = mapM_ definedVar xs
lintExpr (BinOp _ x y ) = definedVar x >> definedVar y
