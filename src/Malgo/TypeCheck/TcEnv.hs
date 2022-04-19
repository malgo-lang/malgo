module Malgo.TypeCheck.TcEnv
  ( RecordTypeName,
    TcEnv (..),
    HasTcEnv (..),
    varEnv,
    typeEnv,
    abbrEnv,
    fieldEnv,
    appendFieldEnv,
    genTcEnv,
    findBuiltinType,
  )
where

import Control.Lens (At (at), Lens', lens, over, traverseOf, traversed, view, _2)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import Koriel.Id
import Koriel.Lens
import Koriel.Pretty
import Malgo.Prelude
import Malgo.Rename.RnEnv (RnEnv)
import qualified Malgo.Rename.RnEnv as R
import Malgo.Syntax.Extension
import Malgo.TypeRep

type RecordTypeName = Text

data TcEnv = TcEnv
  { _varEnv :: HashMap RnId (Scheme Type),
    _typeEnv :: HashMap RnId (TypeDef Type),
    _abbrEnv :: HashMap (Id Type) ([Id Type], Type),
    _fieldEnv :: HashMap RnId [(RecordTypeName, Scheme Type)]
  }
  deriving stock (Show)

varEnv :: Lens' TcEnv (HashMap RnId (Scheme Type))
varEnv = lens _varEnv (\t x -> t {_varEnv = x})

typeEnv :: Lens' TcEnv (HashMap (Id ()) (TypeDef Type))
typeEnv = lens _typeEnv (\t x -> t {_typeEnv = x})

abbrEnv :: Lens' TcEnv (HashMap (Id Type) ([Id Type], Type))
abbrEnv = lens _abbrEnv (\t x -> t {_abbrEnv = x})

fieldEnv :: Lens' TcEnv (HashMap (Id ()) [(RecordTypeName, Scheme Type)])
fieldEnv = lens _fieldEnv (\t x -> t {_fieldEnv = x})

class HasTcEnv env where
  tcEnv :: Lens' env TcEnv

instance HasTcEnv TcEnv where
  tcEnv = identity

instance Pretty TcEnv where
  pPrint TcEnv {..} =
    "TcEnv"
      <+> braces
        ( sep
            [ "_varEnv" <+> "=" <+> pPrint (HashMap.toList _varEnv),
              "_typeEnv" <+> "=" <+> pPrint (HashMap.toList _typeEnv),
              "_abbrEnv" <+> "=" <+> pPrint (HashMap.toList _abbrEnv),
              "_fieldEnv" <+> "=" <+> pPrint (HashMap.toList _fieldEnv)
            ]
        )

instance HasType TcEnv where
  typeOf TcEnv {} = error "typeOf TcEnv{..}"
  types f TcEnv {..} =
    TcEnv <$> traverseOf (traversed . traversed . types) f _varEnv
      <*> traverseOf (traversed . traversed . types) f _typeEnv
      <*> traverseOf (traversed . traversed . types) f _abbrEnv
      <*> traverseOf (traversed . traversed . _2 . traversed . types) f _fieldEnv

appendFieldEnv :: [(Id (), (RecordTypeName, Scheme Type))] -> TcEnv -> TcEnv
appendFieldEnv newEnv = over fieldEnv (go newEnv)
  where
    go [] e = e
    go ((n, n') : xs) e = go xs $ HashMap.alter (f n') n e
    f n' ns = Just $ (n' :) $ concat ns

genTcEnv :: Applicative f => RnEnv -> f TcEnv
genTcEnv rnEnv = do
  let int32_t = fromJust $ findBuiltinType "Int32#" rnEnv
  let int64_t = fromJust $ findBuiltinType "Int64#" rnEnv
  let float_t = fromJust $ findBuiltinType "Float#" rnEnv
  let double_t = fromJust $ findBuiltinType "Double#" rnEnv
  let char_t = fromJust $ findBuiltinType "Char#" rnEnv
  let string_t = fromJust $ findBuiltinType "String#" rnEnv
  pure $
    TcEnv
      { _varEnv = mempty,
        _typeEnv =
          HashMap.fromList
            [ (int32_t, TypeDef (TyPrim Int32T) [] []),
              (int64_t, TypeDef (TyPrim Int64T) [] []),
              (float_t, TypeDef (TyPrim FloatT) [] []),
              (double_t, TypeDef (TyPrim DoubleT) [] []),
              (char_t, TypeDef (TyPrim CharT) [] []),
              (string_t, TypeDef (TyPrim StringT) [] [])
            ],
        _abbrEnv = mempty,
        _fieldEnv = mempty
      }

findBuiltinType :: Text -> RnEnv -> Maybe (Id ())
findBuiltinType x rnEnv = do
  ids <- map (view value) <$> view (R.typeEnv . at x) rnEnv
  find (view sort >>> \case External (ModuleName "Builtin") -> True; _ -> False) ids
