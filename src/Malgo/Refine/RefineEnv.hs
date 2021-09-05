module Malgo.Refine.RefineEnv where

import Koriel.Id
import Koriel.MonadUniq
import Malgo.Infer.TcEnv
import Malgo.Prelude
import Malgo.Rename.RnEnv (RnEnv (..))
import Malgo.TypeRep.Static
import Malgo.TypeRep.UTerm
import qualified Malgo.TypeRep.UTerm as UTerm
import qualified RIO.HashMap as HashMap

data RefineEnv = RefineEnv
  { _typeDefEnv :: HashMap (Id Kind) (TypeDef Type),
    _refineMalgoEnv :: MalgoEnv
  }

typeDefEnv :: Lens' RefineEnv (HashMap (Id Kind) (TypeDef Type))
typeDefEnv = lens _typeDefEnv (\r x -> r {_typeDefEnv = x})

refineMalgoEnv :: Lens' RefineEnv MalgoEnv
refineMalgoEnv = lens _refineMalgoEnv (\r x -> r {_refineMalgoEnv = x})

instance HasMalgoEnv RefineEnv where
  malgoEnv = refineMalgoEnv

instance HasOpt RefineEnv where
  malgoOpt = refineMalgoEnv . malgoOpt

instance HasUniqSupply RefineEnv where
  uniqSupply = refineMalgoEnv . uniqSupply

instance HasLogFunc RefineEnv where
  logFuncL = refineMalgoEnv . logFuncL

buildRefineEnv :: TcEnv -> RefineEnv
buildRefineEnv TcEnv {_typeEnv, _rnEnv = RnEnv {_rnMalgoEnv}} =
  RefineEnv
    { _typeDefEnv = HashMap.fromList $ mapMaybe f $ HashMap.elems _typeEnv,
      _refineMalgoEnv = _rnMalgoEnv
    }
  where
    f :: TypeDef UType -> Maybe (Id Kind, TypeDef Type)
    f t@TypeDef {_typeConstructor = UTerm.TyCon con} = Just (fmap toType con, fmap toType t)
    f _ = Nothing
