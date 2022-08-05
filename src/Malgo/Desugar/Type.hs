module Malgo.Desugar.Type (dsType, unfoldType) where

import Control.Lens ((^.))
import qualified Data.HashMap.Strict as HashMap
import Koriel.Core.Type
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Pretty
import Malgo.Desugar.DsEnv
import Malgo.Prelude
import Malgo.Infer.TypeRep
import qualified Malgo.Infer.TypeRep as GT

-- Malgoの型をCoreの型に変換する
dsType :: Monad m => GT.Type -> m C.Type
dsType t@GT.TyApp {} = dsTyApp [] t
dsType (GT.TyVar _) = pure AnyT
dsType (GT.TyCon con) = do
  case con ^. idMeta of
    GT.TYPE -> pure AnyT
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
dsType (GT.TyTuple 0) = pure $ SumT [C.Con C.Tuple []]
dsType (GT.TyPtr t) = PtrT <$> dsType t
dsType (GT.TyRecord kts) =
  SumT . pure . C.Con C.Tuple . HashMap.elems <$> traverse dsType kts
dsType GT.TyBottom = pure AnyT
dsType GT.TyMeta {} = pure AnyT
dsType t = errorDoc $ "invalid type on dsType:" <+> pPrint t

dsTyApp :: Monad f => [GT.Type] -> GT.Type -> f C.Type
dsTyApp ts (GT.TyTuple _) = SumT . pure . C.Con C.Tuple <$> traverse dsType ts
dsTyApp ts (GT.TyApp t1 t2) = dsTyApp (t2 : ts) t1
dsTyApp _ _ = pure AnyT

-- List aのような型を、<Nil | Cons a (List a)>のような和型に展開する
unfoldType :: MonadState DsEnv m => GT.Type -> m C.Type
unfoldType t@(TyConApp (TyCon con) ts) = do
  case GT.kindOf t of
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
