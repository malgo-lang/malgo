module Malgo.Infer.TcEnv
  ( RecordTypeName,
    TcEnv (..),
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
import Koriel.Pretty
import Malgo.Prelude
import Malgo.Rename.RnEnv (RnEnv)
import qualified Malgo.Rename.RnEnv as R
import Malgo.Syntax.Extension
import Malgo.TypeRep.Static (Scheme, TypeDef (..))
import qualified Malgo.TypeRep.Static as Static
import Malgo.TypeRep.UTerm

type RecordTypeName = Text

data TcEnv = TcEnv
  { _varEnv :: HashMap RnId (Scheme UType),
    _typeEnv :: HashMap RnId (TypeDef UType),
    _abbrEnv :: HashMap (Id UType) ([Id UType], UType),
    _fieldEnv :: HashMap RnId [(RecordTypeName, Scheme UType)]
  }
  deriving stock (Show)

varEnv :: Lens' TcEnv (HashMap RnId (Scheme UType))
varEnv = lens _varEnv (\t x -> t {_varEnv = x})

typeEnv :: Lens' TcEnv (HashMap (Id ()) (TypeDef UType))
typeEnv = lens _typeEnv (\t x -> t {_typeEnv = x})

abbrEnv :: Lens' TcEnv (HashMap (Id UType) ([Id UType], UType))
abbrEnv = lens _abbrEnv (\t x -> t {_abbrEnv = x})

fieldEnv :: Lens' TcEnv (HashMap (Id ()) [(RecordTypeName, Scheme UType)])
fieldEnv = lens _fieldEnv (\t x -> t {_fieldEnv = x})

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

appendFieldEnv :: [(Id (), (RecordTypeName, Scheme UType))] -> TcEnv -> TcEnv
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
            [ (int32_t, TypeDef (TyPrim Static.Int32T) [] []),
              (int64_t, TypeDef (TyPrim Static.Int64T) [] []),
              (float_t, TypeDef (TyPrim Static.FloatT) [] []),
              (double_t, TypeDef (TyPrim Static.DoubleT) [] []),
              (char_t, TypeDef (TyPrim Static.CharT) [] []),
              (string_t, TypeDef (TyPrim Static.StringT) [] [])
            ],
        _abbrEnv = mempty,
        _fieldEnv = mempty
      }

findBuiltinType :: Text -> RnEnv -> Maybe (Id ())
findBuiltinType x rnEnv = do
  ids <- map (view value) <$> view (R.typeEnv . at x) rnEnv
  find (view idSort >>> \case External (ModuleName "Builtin") -> True; _ -> False) ids