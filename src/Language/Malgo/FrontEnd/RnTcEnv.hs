{-# LANGUAGE TemplateHaskell #-}
module Language.Malgo.FrontEnd.RnTcEnv where

import           Control.Lens.TH
import           Language.Malgo.Id
import           Language.Malgo.Monad
import           Language.Malgo.Type
import           Universum            hiding (Type)

data RnTcEnv = RnTcEnv
  { _variableMap  :: Map Id (TypeScheme Id)
  , _builtInMap   :: Map Text Id
  , _typeAliasMap :: Map Text ([Id], Type Id)
  }

makeLenses ''RnTcEnv

makeRnTcEnv :: MalgoM RnTcEnv
makeRnTcEnv = undefined

primitives :: [(Text, TypeScheme Text)]
primitives = [ ("intToString", Forall [] $ intType --> stringType)
             , ("doubleToString", Forall [] $ doubleType --> stringType)
             , ("charToString", Forall [] $ charType --> stringType)
             , ("boolToString", Forall [] $ boolType --> stringType)
             , ("print", Forall [] $ stringType --> unitType)
             , ("println", Forall [] $ stringType --> unitType)
             , ("getChar", Forall [] $ unitType --> charType)
             , ("getLine", Forall [] $ unitType --> stringType)
             , ("getContents", Forall [] $ unitType --> stringType)
             , ("newArray", Forall ["a"]
                 $ intType --> TyVar "a" --> arrayType (TyVar "a"))
             , ("readArray", Forall ["a"]
                 $ arrayType (TyVar "a") --> intType --> TyVar "a")
             , ("writeArray", Forall ["a"]
                 $ arrayType (TyVar "a") --> intType --> TyVar "a" --> unitType)
             ]
