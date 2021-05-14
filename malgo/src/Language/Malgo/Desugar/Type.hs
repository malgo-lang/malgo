{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Desugar.Type (dsType, unfoldType) where

import qualified Data.Map.Strict as Map
import Koriel.Core.Type
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Pretty
import Language.Malgo.Desugar.DsEnv
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.Static
import qualified Language.Malgo.TypeRep.Static as GT

-- Malgoの型をCoreの型に変換する
dsType :: Monad m => GT.Type -> m C.Type
dsType GT.TyApp {} = pure AnyT
dsType (GT.TyVar _) = pure AnyT
dsType (GT.TyCon con) = do
  case con ^. idMeta of
    GT.TYPE (GT.Rep GT.BoxedRep) -> pure AnyT
    kcon -> errorDoc $ "Invalid kind:" <+> pPrint con <+> ":" <+> pPrint kcon
dsType (GT.TyPrim GT.Int32T) = pure C.Int32T
dsType (GT.TyPrim GT.Int64T) = pure C.Int64T
dsType (GT.TyPrim GT.FloatT) = pure C.FloatT
dsType (GT.TyPrim GT.DoubleT) = pure C.DoubleT
dsType (GT.TyPrim GT.CharT) = pure C.CharT
dsType (GT.TyPrim GT.StringT) = pure C.StringT
dsType (GT.TyArr t1 t2) = do
  t1' <- dsType t1
  t2' <- dsType t2
  pure $ [t1'] :-> t2'
dsType (GT.TyTuple ts) =
  SumT . pure . C.Con C.Tuple <$> traverse dsType ts
dsType (GT.TyRecord kts) =
  SumT . pure . C.Con C.Tuple . Map.elems <$> traverse dsType kts
dsType (GT.TyLazy t) = ([] :->) <$> dsType t
dsType (GT.TyPtr t) = PtrT <$> dsType t
dsType t = errorDoc $ "invalid type on dsType:" <+> pPrint t

-- List aのような型を、<Nil | Cons a (List a)>のような和型に展開する
unfoldType :: MonadState DsEnv m => GT.Type -> m C.Type
unfoldType t | GT._TyApp `has` t || GT._TyCon `has` t = do
  GT.kindOf t >>= \case
    TYPE (Rep BoxedRep) -> do
      let (con, ts) = splitCon t
      vcs <- lookupValueConstructors con ts
      SumT
        <$> traverse
          ( \(conName, Forall _ conType) ->
              C.Con (Data $ conName ^. toText) <$> traverse dsType (fst $ splitTyArr conType)
          )
          vcs
    _ -> dsType t
unfoldType t = dsType t
