module Malgo.Desugar.Type (dsType, unfoldType) where

import Control.Lens (mapped, over, (^.))
import qualified Data.Map.Strict as Map
import Koriel.Core.Type
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Pretty
import Malgo.Desugar.DsEnv
import Malgo.Prelude
import Malgo.TypeRep
import qualified Malgo.TypeRep as GT

-- Malgoの型をCoreの型に変換する
dsType :: GT.Type -> C.Type
dsType (GT.TyConApp t ts) = dsTyConApp t ts
dsType (GT.TyVar _) = AnyT
dsType (GT.TyCon con) =
  case con ^. idMeta of
    GT.TYPE (GT.Rep GT.BoxedRep) -> AnyT
    kcon -> errorDoc $ "Invalid kind:" <+> pPrint con <+> ":" <+> pPrint kcon
dsType (GT.TyPrim GT.Int32T) = C.Int32T
dsType (GT.TyPrim GT.Int64T) = C.Int64T
dsType (GT.TyPrim GT.FloatT) = C.FloatT
dsType (GT.TyPrim GT.DoubleT) = C.DoubleT
dsType (GT.TyPrim GT.CharT) = C.CharT
dsType (GT.TyPrim GT.StringT) = C.StringT
dsType (GT.TyArr t1 t2) = [dsType t1] :-> dsType t2
dsType (GT.TyTuple 0) = SumT [C.Con C.Tuple []]
dsType (GT.TyPtr t) = PtrT $ dsType t
dsType (GT.TyRecord kts) =
  SumT [C.Con C.Tuple (Map.elems $ over mapped dsType kts)]
dsType GT.TyBottom = AnyT
dsType GT.TyMeta {} = AnyT
dsType t = errorDoc $ "invalid type on dsType:" <+> pPrint t

dsTyConApp :: GT.Type -> [GT.Type] -> C.Type
dsTyConApp (GT.TyTuple _) ts = SumT [C.Con C.Tuple $ map dsType ts]
dsTyConApp _ _ = AnyT

-- List aのような型を、<Nil | Cons a (List a)>のような和型に展開する
unfoldType :: MonadState DsEnv m => GT.Type -> m C.Type
unfoldType t@(TyConApp (TyCon con) ts) = do
  case GT.kindOf t of
    TYPE (Rep BoxedRep) -> do
      vcs <- lookupValueConstructors con ts
      pure $
        SumT $
          map
            ( \(conName, Forall _ conType) ->
                C.Con (Data $ idToText conName) $ map dsType (fst $ splitTyArr conType)
            )
            vcs
    _ -> pure (dsType t)
unfoldType t = pure (dsType t)
