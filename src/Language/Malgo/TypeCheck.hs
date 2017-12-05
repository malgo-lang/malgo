{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.TypeCheck where

import qualified Data.Map.Strict       as Map
import           Language.Malgo.Rename
import           Language.Malgo.Syntax
import           Language.Malgo.Utils
import           Text.PrettyPrint

data TypedID = TypedID ID Type

instance PrettyPrint TypedID where
  pretty (TypedID x t) = pretty x <> text ":" <> pretty t

newtype TcEnv = TcEnv { table :: Map.Map ID TypedID }

initTcEnv :: TcEnv
initTcEnv = TcEnv
  { table = Map.fromList env }
  where
        env = map (\(x, t) -> (x, TypedID x t))
              [ ( External "print" 0
                , FunTy (NameTy "String") (NameTy "Unit"))
              , ( External "println" 1
                , FunTy (NameTy "String") (NameTy "Unit"))
              , ( External "print_int" 2
                , FunTy (NameTy "Int") (NameTy "Unit"))
              , ( External "print_float" 3
                , FunTy (NameTy "Float") (NameTy "Unit"))
              , ( External "print_bool" 4
                , FunTy (NameTy "Bool") (NameTy "Unit"))
              , ( External "print_char" 5
                , FunTy (NameTy "Char") (NameTy "Unit"))
              ]
