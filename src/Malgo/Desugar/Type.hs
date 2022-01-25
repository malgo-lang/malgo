module Malgo.Desugar.Type (dsType, unfoldType) where

import Control.Lens ((^.))
import qualified Data.Map.Strict as Map
import Koriel.Core.Type
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Pretty
import Malgo.Desugar.DsEnv
import Malgo.Prelude
import Malgo.TypeRep
import qualified Malgo.TypeRep as GT

-- | Convert Malgo types to Core types
dsType :: Monad m => GT.Type -> m C.Type
dsType (GT.TyApp (GT.TyTuple _) (toList -> ts)) = SumT . pure . C.Con C.Tuple <$> traverse dsType ts
dsType (GT.TyApp _ _) = pure AnyT
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
dsType (GT.TyTuple 0) = pure $ SumT [C.Con C.Tuple []]
dsType (GT.TyPtr t) = PtrT <$> dsType t
dsType (GT.TyRecord kts) =
  SumT . pure . C.Con C.Tuple . Map.elems <$> traverse dsType kts
dsType GT.TyBottom = pure AnyT
dsType GT.TyMeta {} = pure AnyT
dsType t = errorDoc $ "invalid type on dsType:" <+> pPrint t

-- | Expand a type such as 'List a' into a union type such as 'Nil | Cons a (List a)'.
unfoldType :: MonadState DsEnv m => GT.Type -> m C.Type
unfoldType t@(TyApp (TyCon con) (toList -> ts)) = do
  case GT.kindOf t of
    TYPE (Rep BoxedRep) -> unfoldTyCon con ts
    _ -> dsType t
unfoldType t@(TyCon con) = do
  case GT.kindOf t of
    TYPE (Rep BoxedRep) -> unfoldTyCon con []
    _ -> dsType t
unfoldType t = dsType t

-- | Expand a type constructor and apply its arguments.
-- The result type's kind must be TYPE (Rep BoxedRep).
unfoldTyCon :: MonadState DsEnv m => Id GT.Type -> [GT.Type] -> m C.Type
unfoldTyCon con ts = do
  vcs <- lookupValueConstructors con ts
  SumT
    <$> traverse
      ( \(conName, Forall _ conType) ->
          C.Con (Data $ idToText conName) <$> traverse dsType (fst $ splitTyArr conType)
      )
      vcs