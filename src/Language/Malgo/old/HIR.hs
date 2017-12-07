{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.HIR where

import           Data.String
import           Language.Malgo.TypeCheck (TypedID)
import qualified Language.Malgo.TypeCheck as T
import           Language.Malgo.Utils
import           Text.PrettyPrint

type HIR = [Instr]

data Instr = Instr TypedID Val

data Val = Int Integer
         | Float Integer
         | Bool Bool
         | Char Char
         | String String
         | Unit
         | Call TypedID [TypedID]
