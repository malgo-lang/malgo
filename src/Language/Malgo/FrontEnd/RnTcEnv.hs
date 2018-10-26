{-# LANGUAGE DeriveGeneric   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Language.Malgo.FrontEnd.RnTcEnv where

import           Control.Lens.TH
import qualified Data.Map.Strict      as Map
import           Data.Outputable
import           Language.Malgo.Id
import           Language.Malgo.Monad
import           Language.Malgo.Type
import           Universum            hiding (Type)

data RnTcEnv = RnTcEnv
  { _variableMap  :: Map Id (TypeScheme Id)
  , _builtInMap   :: Map Text Id
  , _typeAliasMap :: Map Text ([Id], Type Id)
  }
  deriving (Show, Generic)

makeLenses ''RnTcEnv

instance Outputable RnTcEnv

makeRnTcEnv :: MalgoM RnTcEnv
makeRnTcEnv = do
  pf <- primFunc
  pt <- primType
  return RnTcEnv
    { _variableMap = Map.fromList $ map (\(_, n', t) -> (n', t)) pf
    , _builtInMap = Map.fromList $ map (\(n, n', _) -> (n, n')) pf
    , _typeAliasMap = Map.fromList pt
    }

primFunc :: MalgoM [(Text, Id, TypeScheme Id)]
primFunc = do
  intToString <- newId "intToString"
  doubleToString <- newId "doubleToString"
  charToString <- newId "charToString"
  boolToString <- newId "boolToString"
  print <- newId "print"
  println <- newId "println"
  getChar <- newId "getChar"
  getLine <- newId "getLine"
  getContents <- newId "getContents"
  newArray <- newId "newArray"
  readArray <- newId "readArray"
  writeArray <- newId "writeArray"

  a0 <- newId "a"
  a1 <- newId "a"
  a2 <- newId "a"

  return [ ("intToString", intToString, Forall [] $ intType --> stringType)
         , ("doubleToString", doubleToString, Forall [] $ doubleType --> stringType)
         , ("charToString", charToString, Forall [] $ charType --> stringType)
         , ("boolToString", boolToString, Forall [] $ boolType --> stringType)
         , ("print", print, Forall [] $ stringType --> unitType)
         , ("println", println, Forall [] $ stringType --> unitType)
         , ("getChar", getChar, Forall [] $ unitType --> charType)
         , ("getLine", getLine, Forall [] $ unitType --> stringType)
         , ("getContents", getContents, Forall [] $ unitType --> stringType)
         , ("newArray", newArray,
             Forall [a0]
             $ intType --> TyVar a0 --> arrayType (TyVar a0))
         , ("readArray", readArray,
             Forall [a1]
             $ arrayType (TyVar a1) --> intType --> TyVar a1)
         , ("writeArray", writeArray,
             Forall [a2]
             $ arrayType (TyVar a2) --> intType --> TyVar a2 --> unitType)
         ]

primType :: MalgoM [(Text, ([Id], Type Id))]
primType = do
  a <- newId "a"
  return [ ("Int", ([], intType))
         , ("Double", ([], doubleType))
         , ("Char", ([], charType))
         , ("Bool", ([], boolType))
         , ("String", ([], stringType))
         , ("Array", ([a], arrayType (TyVar a)))
         ]
