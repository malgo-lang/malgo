module Malgo.Desugar.Type (dsType, unfoldType) where

import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, gets)
import Malgo.Core.Type
import Malgo.Core.Type qualified as C
import Malgo.Desugar.DsState
import Malgo.Id
import Malgo.Infer.TypeRep
import Malgo.Infer.TypeRep qualified as GT
import Malgo.Prelude

-- Malgoの型をCoreの型に変換する
dsType :: (State DsState :> es) => GT.Type -> Eff es C.Type
dsType t@GT.TyApp {} = dsTyApp [] t
dsType (GT.TyVar _) = pure AnyT
dsType (GT.TyCon con) = do
  ctx <- gets @DsState (._kindCtx)
  case kindOf ctx con of
    GT.TYPE -> pure AnyT
    kcon -> errorDoc $ "Invalid kind:" <+> pretty con <+> ":" <+> pretty kcon
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
dsType (GT.TyTuple 0) = pure $ SumT [C.Con C.Tuple []]
dsType GT.TyPtr = errorDoc "unreachable"
dsType (GT.TyRecord kts) = RecordT <$> traverse dsType kts
dsType GT.TyMeta {} = pure AnyT
dsType t = errorDoc $ "invalid type on dsType:" <+> pretty t

dsTyApp :: (State DsState :> es) => [GT.Type] -> GT.Type -> Eff es C.Type
dsTyApp ts (GT.TyTuple _) = SumT . pure . C.Con C.Tuple <$> traverse dsType ts
dsTyApp ts (GT.TyApp t1 t2) = dsTyApp (t2 : ts) t1
dsTyApp _ _ = pure AnyT

-- List aのような型を、<Nil | Cons a (List a)>のような和型に展開する
unfoldType :: (State DsState :> es) => GT.Type -> Eff es C.Type
unfoldType t@(TyConApp (TyCon con) ts) = do
  ctx <- gets @DsState (._kindCtx)
  case GT.kindOf ctx t of
    TYPE -> do
      vcs <- lookupValueConstructors con ts
      SumT
        <$> traverse
          ( \(conName, Forall _ conType) ->
              C.Con (Data $ idToText conName) <$> traverse dsType (fst $ splitTyArr conType)
          )
          vcs
    _ -> dsType t
unfoldType t = dsType t
