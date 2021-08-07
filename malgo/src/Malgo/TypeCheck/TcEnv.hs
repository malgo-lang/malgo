{-# LANGUAGE TemplateHaskell #-}

module Malgo.TypeCheck.TcEnv
  ( RecordTypeName,
    TcEnv (..),
    varEnv,
    typeEnv,
    abbrEnv,
    fieldEnv,
    rnEnv,
    appendFieldEnv,
    genTcEnv,
    findBuiltinType,
  )
where

import Data.Foldable (find)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import Koriel.Id
import Koriel.Pretty
import Malgo.Prelude
import Malgo.Rename.RnEnv (RnEnv)
import qualified Malgo.Rename.RnEnv as R
import Malgo.Syntax.Extension
import Malgo.TypeRep.Static (Scheme, TypeDef (..), TypeF)
import qualified Malgo.TypeRep.Static as Static
import Malgo.TypeRep.UTerm
import Malgo.UTerm

type RecordTypeName = String

data TcEnv = TcEnv
  { _varEnv :: HashMap RnId (Scheme UType),
    _typeEnv :: HashMap RnId (TypeDef UType),
    _abbrEnv :: HashMap (Id UType) ([Id UType], UType),
    _fieldEnv :: HashMap RnId [(RecordTypeName, Scheme UType)],
    _rnEnv :: RnEnv
  }
  deriving stock (Show)

makeLenses ''TcEnv

instance Pretty TcEnv where
  pretty TcEnv {..} =
    "TcEnv"
      <+> braces
        ( sep
            [ "_varEnv" <+> "=" <+> pretty (HashMap.toList _varEnv),
              "_typeEnv" <+> "=" <+> pretty (HashMap.toList _typeEnv),
              "_abbrEnv" <+> "=" <+> pretty (HashMap.toList _abbrEnv),
              "_fieldEnv" <+> "=" <+> pretty (HashMap.toList _fieldEnv),
              "_rnEnv" <+> "=" <+> pretty _rnEnv
            ]
        )

instance HasUTerm TypeF TypeVar TcEnv where
  walkOn f TcEnv {..} =
    TcEnv <$> traverseOf (traversed . traversed . walkOn) f _varEnv
      <*> traverseOf (traversed . traversed . walkOn) f _typeEnv
      <*> traverseOf (traversed . traversed . walkOn) f _abbrEnv
      <*> traverseOf (traversed . traversed . _2 . traversed . walkOn) f _fieldEnv
      <*> pure _rnEnv

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
        _fieldEnv = mempty,
        _rnEnv = rnEnv
      }

findBuiltinType :: String -> RnEnv -> Maybe (Id ())
findBuiltinType x rnEnv = do
  ids <- map _value <$> view (R.typeEnv . at x) rnEnv
  find (view idSort >>> \case WiredIn (ModuleName "Builtin") -> True; _ -> False) ids
